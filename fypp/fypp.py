#!/usr/bin/env python3
# -*- coding: utf-8 -*-
################################################################################
#
# fypp -- Python powered Fortran preprocessor
#
# Copyright (c) 2016-2023 Bálint Aradi, Universität Bremen
#
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# 1. Redistributions of source code must retain the above copyright notice, this
# list of conditions and the following disclaimer.
#
# 2. Redistributions in binary form must reproduce the above copyright notice,
# this list of conditions and the following disclaimer in the documentation
# and/or other materials provided with the distribution.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS 'AS IS'
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
# DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
# FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
# DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
# SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
# OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#
################################################################################

'''For using the functionality of the Fypp preprocessor from within
Python, one usually interacts with the following two classes:

* `Fypp`_: The actual Fypp preprocessor. It returns for a given input
  the preprocessed output.

* `FyppOptions`_: Contains customizable settings controlling the behaviour of
  `Fypp`_. Alternatively, the function `get_option_parser()`_ can be used to
  obtain an option parser, which can create settings based on command line
  arguments.

If processing stops prematurely, an instance of one of the following
subclasses of `FyppError`_ is raised:

* FyppFatalError: Unexpected error (e.g. bad input, missing files, etc.)

* FyppStopRequest: Stop was triggered by an explicit request in the input
  (by a stop- or an assert-directive).
'''
import pathlib
import sys
import types
import inspect
import re
import os
import errno
import time
import optparse
import io
import platform
import builtins

# Prevent cluttering user directory with Python bytecode
sys.dont_write_bytecode = True

VERSION = '3.2'

STDIN = '<stdin>'

FILEOBJ = '<fileobj>'

STRING = '<string>'

ERROR_EXIT_CODE = 1

USER_ERROR_EXIT_CODE = 2

_ALL_DIRECTIVES_PATTERN = r'''
# comment block
(?:^[ \t]*\#!.*\n)+
|
# line directive (with optional continuation lines)
^[ \t]*(?P<ldirtype>[\#\$@]):[ \t]*
(?P<ldir>.+?(?:&[ \t]*\n(?:[ \t]*&)?.*?)*)?[ \t]*\n
|
# inline eval directive
(?P<idirtype>[$\#@])\{[ \t]*(?P<idir>.+?)?[ \t]*\}(?P=idirtype)
'''

_ALL_DIRECTIVES_REGEXP = re.compile(
    _ALL_DIRECTIVES_PATTERN, re.VERBOSE | re.MULTILINE)

_CONTROL_DIR_REGEXP = re.compile(
    r'(?P<dir>[a-zA-Z_]\w*)[ \t]*(?:[ \t]+(?P<param>[^ \t].*))?$')

_DIRECT_CALL_REGEXP = re.compile(
    r'(?P<callname>[a-zA-Z_][\w.]*)[ \t]*\((?P<callparams>.+?)?\)$')

_DIRECT_CALL_KWARG_REGEXP = re.compile(
    r'(?:(?P<kwname>[a-zA-Z_]\w*)\s*=(?=[^=]|$))?')

_DEF_PARAM_REGEXP = re.compile(
    r'^(?P<name>[a-zA-Z_]\w*)[ \t]*\(\s*(?P<args>.+)?\s*\)$')

_SIMPLE_CALLABLE_REGEXP = re.compile(
    r'^(?P<name>[a-zA-Z_][\w.]*)[ \t]*(?:\([ \t]*(?P<args>.*)[ \t]*\))?$')

_IDENTIFIER_NAME_REGEXP = re.compile(r'^(?P<name>[a-zA-Z_]\w*)$')

_PREFIXED_IDENTIFIER_NAME_REGEXP = re.compile(r'^(?P<name>[a-zA-Z_][\w.]*)$')

_SET_PARAM_REGEXP = re.compile(
    r'^(?P<name>(?:[(]\s*)?[a-zA-Z_]\w*(?:\s*,\s*[a-zA-Z_]\w*)*(?:\s*[)])?)\s*'\
    r'(?:=\s*(?P<expr>.*))?$')

_DEL_PARAM_REGEXP = re.compile(
    r'^(?:[(]\s*)?[a-zA-Z_]\w*(?:\s*,\s*[a-zA-Z_]\w*)*(?:\s*[)])?$')

_FOR_PARAM_REGEXP = re.compile(
    r'^(?P<loopexpr>[a-zA-Z_]\w*(\s*,\s*[a-zA-Z_]\w*)*)\s+in\s+(?P<iter>.+)$')

_INCLUDE_PARAM_REGEXP = re.compile(r'^(\'|")(?P<fname>.*?)\1$')

_COMMENTLINE_REGEXP = re.compile(r'^[ \t]*!.*$')

_CONTLINE_REGEXP = re.compile(r'&[ \t]*\n(?:[ \t]*&)?')

_UNESCAPE_TEXT_REGEXP1 = re.compile(r'([$#@])\\(\\*)([{:])')

_UNESCAPE_TEXT_REGEXP2 = re.compile(r'#\\(\\*)([!])')

_UNESCAPE_TEXT_REGEXP3 = re.compile(r'(\})\\(\\*)([$#@])')

_INLINE_EVAL_REGION_REGEXP = re.compile(r'\${.*?}\$')

_RESERVED_PREFIX = '__'

_RESERVED_NAMES = set(['defined', 'setvar', 'getvar', 'delvar', 'globalvar',
                       '_LINE_', '_FILE_', '_THIS_FILE_', '_THIS_LINE_',
                       '_TIME_', '_DATE_', '_SYSTEM_', '_MACHINE_'])

_LINENUM_NEW_FILE = 1

_LINENUM_RETURN_TO_FILE = 2

_QUOTES_FORTRAN = '\'"'

_OPENING_BRACKETS_FORTRAN = '{(['

_CLOSING_BRACKETS_FORTRAN = '})]'

_ARGUMENT_SPLIT_CHAR_FORTRAN = ','


class FyppError(Exception):
    '''Signalizes error occurring during preprocessing.

    Args:
        msg (str): Error message.
        fname (str): File name. None (default) if file name is not available.
        span (tuple of int): Beginning and end line of the region where error
            occurred or None if not available. If fname was not None, span must
            not be None.

    Attributes:
        msg (str): Error message.
        fname (str or None): File name or None if not available.
        span (tuple of int or None): Beginning and end line of the region
            where error occurred or None if not available. Line numbers start
            from zero. For directives, which do not consume end of the line,
            start and end lines are identical.
    '''

    def __init__(self, msg, fname=None, span=None):
        super().__init__()
        self.msg = msg
        self.fname = fname
        self.span = span


    def __str__(self):
        msg = [self.__class__.__name__, ': ']
        if self.fname is not None:
            msg.append("file '" + self.fname + "'")
            if self.span[1] > self.span[0] + 1:
                msg.append(', lines {0}-{1}'.format(
                    self.span[0] + 1, self.span[1]))
            else:
                msg.append(', line {0}'.format(self.span[0] + 1))
            msg.append('\n')
        if self.msg:
            msg.append(self.msg)
        if self.__cause__ is not None:
            msg.append('\n' + str(self.__cause__))
        return ''.join(msg)


class FyppFatalError(FyppError):
    '''Signalizes an unexpected error during processing.'''


class FyppStopRequest(FyppError):
    '''Signalizes an explicitely triggered stop (e.g. via stop directive)'''


class Parser:
    '''Parses a text and generates events when encountering Fypp constructs.

    Args:
        includedirs (list): List of directories, in which include files should
            be searched for, when they are not found at the default location.

        encoding (str): Encoding to use when reading the file (default: utf-8)
    '''

    def __init__(self, includedirs=None, encoding='utf-8'):

        # Directories to search for include files
        if includedirs is None:
            self._includedirs = []
        else:
            self._includedirs = includedirs

        # Encoding
        self._encoding = encoding

        # Name of current file
        self._curfile = None

        # Directory of current file
        self._curdir = None


    def parsefile(self, fobj):
        '''Parses file or a file like object.

        Args:
            fobj (str or file): Name of a file or a file like object.
        '''
        if isinstance(fobj, str):
            if fobj == STDIN:
                self._includefile(None, sys.stdin, STDIN, os.getcwd())
            else:
                inpfp = _open_input_file(fobj, self._encoding)
                self._includefile(None, inpfp, fobj, os.path.dirname(fobj))
                inpfp.close()
        else:
            self._includefile(None, fobj, FILEOBJ, os.getcwd())


    def _includefile(self, span, fobj, fname, curdir):
        oldfile = self._curfile
        olddir = self._curdir
        self._curfile = fname
        self._curdir = curdir
        self._parse_txt(span, fname, fobj.read())
        self._curfile = oldfile
        self._curdir = olddir


    def parse(self, txt):
        '''Parses string.

        Args:
            txt (str): Text to parse.
        '''
        self._curfile = STRING
        self._curdir = ''
        self._parse_txt(None, self._curfile, txt)


    def handle_include(self, span, fname):
        '''Called when parser starts to process a new file.

        It is a dummy methond and should be overridden for actual use.

        Args:
            span (tuple of int): Start and end line of the include directive
                or None if called the first time for the main input.
            fname (str): Name of the file.
        '''
        self._log_event('include', span, filename=fname)


    def handle_endinclude(self, span, fname):
        '''Called when parser finished processing a file.

        It is a dummy method and should be overridden for actual use.

        Args:
            span (tuple of int): Start and end line of the include directive
                or None if called the first time for the main input.
            fname (str): Name of the file.
        '''
        self._log_event('endinclude', span, filename=fname)


    def handle_set(self, span, name, expr):
        '''Called when parser encounters a set directive.

        It is a dummy method and should be overridden for actual use.

        Args:
            span (tuple of int): Start and end line of the directive.
            name (str): Name of the variable.
            expr (str): String representation of the expression to be assigned
                to the variable.
        '''
        self._log_event('set', span, name=name, expression=expr)


    def handle_def(self, span, name, args):
        '''Called when parser encounters a def directive.

        It is a dummy method and should be overridden for actual use.

        Args:
            span (tuple of int): Start and end line of the directive.
            name (str): Name of the macro to be defined.
            argexpr (str): String with argument definition (or None)
        '''
        self._log_event('def', span, name=name, arguments=args)


    def handle_enddef(self, span, name):
        '''Called when parser encounters an enddef directive.

        It is a dummy method and should be overridden for actual use.

        Args:
            span (tuple of int): Start and end line of the directive.
            name (str): Name found after the enddef directive.
        '''
        self._log_event('enddef', span, name=name)


    def handle_del(self, span, name):
        '''Called when parser encounters a del directive.

        It is a dummy method and should be overridden for actual use.

        Args:
            span (tuple of int): Start and end line of the directive.
            name (str): Name of the variable to delete.
        '''
        self._log_event('del', span, name=name)


    def handle_if(self, span, cond):
        '''Called when parser encounters an if directive.

        It is a dummy method and should be overridden for actual use.

        Args:
            span (tuple of int): Start and end line of the directive.
            cond (str): String representation of the branching condition.
        '''
        self._log_event('if', span, condition=cond)


    def handle_elif(self, span, cond):
        '''Called when parser encounters an elif directive.

        It is a dummy method and should be overridden for actual use.

        Args:
            span (tuple of int): Start and end line of the directive.
            cond (str): String representation of the branching condition.
        '''
        self._log_event('elif', span, condition=cond)


    def handle_else(self, span):
        '''Called when parser encounters an else directive.

        It is a dummy method and should be overridden for actual use.

        Args:
            span (tuple of int): Start and end line of the directive.
        '''
        self._log_event('else', span)


    def handle_endif(self, span):
        '''Called when parser encounters an endif directive.

        It is a dummy method and should be overridden for actual use.

        Args:
            span (tuple of int): Start and end line of the directive.
        '''
        self._log_event('endif', span)


    def handle_for(self, span, varexpr, iterator):
        '''Called when parser encounters a for directive.

        It is a dummy method and should be overridden for actual use.

        Args:
            span (tuple of int): Start and end line of the directive.
            varexpr (str): String representation of the loop variable
                expression.
            iterator (str): String representation of the iterable.
        '''
        self._log_event('for', span, variable=varexpr, iterable=iterator)


    def handle_endfor(self, span):
        '''Called when parser encounters an endfor directive.

        It is a dummy method and should be overridden for actual use.

        Args:
            span (tuple of int): Start and end line of the directive.
        '''
        self._log_event('endfor', span)


    def handle_call(self, span, name, argexpr, blockcall):
        '''Called when parser encounters a call directive.

        It is a dummy method and should be overridden for actual use.

        Args:
            span (tuple of int): Start and end line of the directive.
            name (str): Name of the callable to call
            argexpr (str or None): Argument expression containing additional
                arguments for the call.
            blockcall (bool): Whether the alternative "block / contains /
                endblock" calling directive has been used.
        '''
        self._log_event('call', span, name=name, argexpr=argexpr,
                        blockcall=blockcall)


    def handle_nextarg(self, span, name, blockcall):
        '''Called when parser encounters a nextarg directive.

        It is a dummy method and should be overridden for actual use.

        Args:
            span (tuple of int): Start and end line of the directive.
            name (str or None): Name of the argument following next or
                None if it should be the next positional argument.
            blockcall (bool): Whether the alternative "block / contains /
                endblock" calling directive has been used.
        '''
        self._log_event('nextarg', span, name=name, blockcall=blockcall)


    def handle_endcall(self, span, name, blockcall):
        '''Called when parser encounters an endcall directive.

        It is a dummy method and should be overridden for actual use.

        Args:
            span (tuple of int): Start and end line of the directive.
            name (str): Name found after the endcall directive.
            blockcall (bool): Whether the alternative "block / contains /
                endblock" calling directive has been used.
        '''
        self._log_event('endcall', span, name=name, blockcall=blockcall)


    def handle_eval(self, span, expr):
        '''Called when parser encounters an eval directive.

        It is a dummy method and should be overridden for actual use.

        Args:
            span (tuple of int): Start and end line of the directive.
            expr (str): String representation of the Python expression to
                be evaluated.
        '''
        self._log_event('eval', span, expression=expr)


    def handle_global(self, span, name):
        '''Called when parser encounters a global directive.

        It is a dummy method and should be overridden for actual use.

        Args:
            span (tuple of int): Start and end line of the directive.
            name (str): Name of the variable which should be made global.
        '''
        self._log_event('global', span, name=name)


    def handle_text(self, span, txt):
        '''Called when parser finds text which must left unaltered.

        It is a dummy method and should be overridden for actual use.

        Args:
            span (tuple of int): Start and end line of the directive.
            txt (str): Text.
        '''
        self._log_event('text', span, content=txt)


    def handle_comment(self, span):
        '''Called when parser finds a preprocessor comment.

        It is a dummy method and should be overridden for actual use.

        Args:
            span (tuple of int): Start and end line of the directive.
        '''
        self._log_event('comment', span)


    def handle_mute(self, span):
        '''Called when parser finds a mute directive.

        It is a dummy method and should be overridden for actual use.

        Args:
            span (tuple of int): Start and end line of the directive.
        '''
        self._log_event('mute', span)


    def handle_endmute(self, span):
        '''Called when parser finds an endmute directive.

        It is a dummy method and should be overridden for actual use.

        Args:
            span (tuple of int): Start and end line of the directive.
        '''
        self._log_event('endmute', span)


    def handle_stop(self, span, msg):
        '''Called when parser finds an stop directive.

        It is a dummy method and should be overridden for actual use.

        Args:
            span (tuple of int): Start and end line of the directive.
            msg (str): Stop message.
        '''
        self._log_event('stop', span, msg=msg)


    def handle_assert(self, span):
        '''Called when parser finds an assert directive.

        It is a dummy method and should be overridden for actual use.

        Args:
            span (tuple of int): Start and end line of the directive.
        '''
        self._log_event('assert', span)


    @staticmethod
    def _log_event(event, span=(-1, -1), **params):
        print('{0}: {1} --> {2}'.format(event, span[0], span[1]))
        for parname, parvalue in params.items():
            print('  {0}: ->|{1}|<-'.format(parname, parvalue))
        print()


    def _parse_txt(self, includespan, fname, txt):
        self.handle_include(includespan, fname)
        self._parse(txt)
        self.handle_endinclude(includespan, fname)


    def _parse(self, txt, linenr=0, directcall=False):
        pos = 0
        for match in _ALL_DIRECTIVES_REGEXP.finditer(txt):
            start, end = match.span()
            if start > pos:
                endlinenr = linenr + txt.count('\n', pos, start)
                self._process_text(txt[pos:start], (linenr, endlinenr))
                linenr = endlinenr
            endlinenr = linenr + txt.count('\n', start, end)
            span = (linenr, endlinenr)
            ldirtype, ldir, idirtype, idir = match.groups()
            if directcall and (idirtype is None or idirtype != '$'):
                msg = 'only inline eval directives allowed in direct calls'
                raise FyppFatalError(msg, self._curfile, span)
            elif idirtype is not None:
                if idir is None:
                    msg = 'missing inline directive content'
                    raise FyppFatalError(msg, self._curfile, span)
                dirtype = idirtype
                content = idir
            elif ldirtype is not None:
                if ldir is None:
                    msg = 'missing line directive content'
                    raise FyppFatalError(msg, self._curfile, span)
                dirtype = ldirtype
                content = _CONTLINE_REGEXP.sub('', ldir)
            else:
                # Comment directive
                dirtype = None
            if dirtype == '$':
                self.handle_eval(span, content)
            elif dirtype == '#':
                self._process_control_dir(content, span)
            elif dirtype == '@':
                self._process_direct_call(content, span)
            else:
                self.handle_comment(span)
            pos = end
            linenr = endlinenr
        if pos < len(txt):
            endlinenr = linenr + txt.count('\n', pos)
            self._process_text(txt[pos:], (linenr, endlinenr))


    def _process_text(self, txt, span):
        escaped_txt = self._unescape(txt)
        self.handle_text(span, escaped_txt)


    def _process_control_dir(self, content, span):
        match = _CONTROL_DIR_REGEXP.match(content)
        if not match:
            msg = "invalid control directive content '{0}'".format(content)
            raise FyppFatalError(msg, self._curfile, span)
        directive, param = match.groups()
        if directive == 'if':
            self._check_param_presence(True, 'if', param, span)
            self.handle_if(span, param)
        elif directive == 'else':
            self._check_param_presence(False, 'else', param, span)
            self.handle_else(span)
        elif directive == 'elif':
            self._check_param_presence(True, 'elif', param, span)
            self.handle_elif(span, param)
        elif directive == 'endif':
            self._check_param_presence(False, 'endif', param, span)
            self.handle_endif(span)
        elif directive == 'def':
            self._check_param_presence(True, 'def', param, span)
            self._check_not_inline_directive('def', span)
            self._process_def(param, span)
        elif directive == 'enddef':
            self._process_enddef(param, span)
        elif directive == 'set':
            self._check_param_presence(True, 'set', param, span)
            self._process_set(param, span)
        elif directive == 'del':
            self._check_param_presence(True, 'del', param, span)
            self._process_del(param, span)
        elif directive == 'for':
            self._check_param_presence(True, 'for', param, span)
            self._process_for(param, span)
        elif directive == 'endfor':
            self._check_param_presence(False, 'endfor', param, span)
            self.handle_endfor(span)
        elif directive == 'call' or directive == 'block':
            self._check_param_presence(True, directive, param, span)
            self._process_call(param, span, directive == 'block')
        elif directive == 'nextarg' or directive == 'contains':
            self._process_nextarg(param, span, directive == 'contains')
        elif directive == 'endcall' or directive == 'endblock':
            self._process_endcall(param, span, directive == 'endblock')
        elif directive == 'include':
            self._check_param_presence(True, 'include', param, span)
            self._check_not_inline_directive('include', span)
            self._process_include(param, span)
        elif directive == 'mute':
            self._check_param_presence(False, 'mute', param, span)
            self._check_not_inline_directive('mute', span)
            self.handle_mute(span)
        elif directive == 'endmute':
            self._check_param_presence(False, 'endmute', param, span)
            self._check_not_inline_directive('endmute', span)
            self.handle_endmute(span)
        elif directive == 'stop':
            self._check_param_presence(True, 'stop', param, span)
            self._check_not_inline_directive('stop', span)
            self.handle_stop(span, param)
        elif directive == 'assert':
            self._check_param_presence(True, 'assert', param, span)
            self._check_not_inline_directive('assert', span)
            self.handle_assert(span, param)
        elif directive == 'global':
            self._check_param_presence(True, 'global', param, span)
            self._process_global(param, span)
        else:
            msg = "unknown directive '{0}'".format(directive)
            raise FyppFatalError(msg, self._curfile, span)


    def _process_direct_call(self, callexpr, span):
        match = _DIRECT_CALL_REGEXP.match(callexpr)
        if not match:
            msg = "invalid direct call expression"
            raise FyppFatalError(msg, self._curfile, span)
        callname = match.group('callname')
        self.handle_call(span, callname, None, False)
        callparams = match.group('callparams')
        if callparams is None or not callparams.strip():
            args = []
        else:
            try:
                args = [arg.strip() for arg in _argsplit_fortran(callparams)]
            except Exception as exc:
                msg = 'unable to parse direct call argument'
                raise FyppFatalError(msg, self._curfile, span) from exc
        for arg in args:
            match = _DIRECT_CALL_KWARG_REGEXP.match(arg)
            argval = arg[match.end():].strip()
            # Remove enclosing braces if present
            if argval.startswith('{'):
                argval = argval[1:-1]
            keyword = match.group('kwname')
            self.handle_nextarg(span, keyword, False)
            self._parse(argval, linenr=span[0], directcall=True)
        self.handle_endcall(span, callname, False)


    def _process_def(self, param, span):
        match = _DEF_PARAM_REGEXP.match(param)
        if not match:
            msg = "invalid macro definition '{0}'".format(param)
            raise FyppFatalError(msg, self._curfile, span)
        name = match.group('name')
        argexpr = match.group('args')
        self.handle_def(span, name, argexpr)


    def _process_enddef(self, param, span):
        if param is not None:
            match = _IDENTIFIER_NAME_REGEXP.match(param)
            if not match:
                msg = "invalid enddef parameter '{0}'".format(param)
                raise FyppFatalError(msg, self._curfile, span)
            param = match.group('name')
        self.handle_enddef(span, param)


    def _process_set(self, param, span):
        match = _SET_PARAM_REGEXP.match(param)
        if not match:
            msg = "invalid variable assignment '{0}'".format(param)
            raise FyppFatalError(msg, self._curfile, span)
        self.handle_set(span, match.group('name'), match.group('expr'))


    def _process_global(self, param, span):
        match = _DEL_PARAM_REGEXP.match(param)
        if not match:
            msg = "invalid variable specification '{0}'".format(param)
            raise FyppFatalError(msg, self._curfile, span)
        self.handle_global(span, param)


    def _process_del(self, param, span):
        match = _DEL_PARAM_REGEXP.match(param)
        if not match:
            msg = "invalid variable specification '{0}'".format(param)
            raise FyppFatalError(msg, self._curfile, span)
        self.handle_del(span, param)


    def _process_for(self, param, span):
        match = _FOR_PARAM_REGEXP.match(param)
        if not match:
            msg = "invalid for loop declaration '{0}'".format(param)
            raise FyppFatalError(msg, self._curfile, span)
        loopexpr = match.group('loopexpr')
        loopvars = [s.strip() for s in loopexpr.split(',')]
        self.handle_for(span, loopvars, match.group('iter'))


    def _process_call(self, param, span, blockcall):
        match = _SIMPLE_CALLABLE_REGEXP.match(param)
        if not match:
            msg = "invalid callable expression '{}'".format(param)
            raise FyppFatalError(msg, self._curfile, span)
        name, args = match.groups()
        self.handle_call(span, name, args, blockcall)


    def _process_nextarg(self, param, span, blockcall):
        if param is not None:
            match = _IDENTIFIER_NAME_REGEXP.match(param)
            if not match:
                msg = "invalid nextarg parameter '{0}'".format(param)
                raise FyppFatalError(msg, self._curfile, span)
            param = match.group('name')
        self.handle_nextarg(span, param, blockcall)


    def _process_endcall(self, param, span, blockcall):
        if param is not None:
            match = _PREFIXED_IDENTIFIER_NAME_REGEXP.match(param)
            if not match:
                msg = "invalid endcall parameter '{0}'".format(param)
                raise FyppFatalError(msg, self._curfile, span)
            param = match.group('name')
        self.handle_endcall(span, param, blockcall)


    def _process_include(self, param, span):
        match = _INCLUDE_PARAM_REGEXP.match(param)
        if not match:
            msg = "invalid include file declaration '{0}'".format(param)
            raise FyppFatalError(msg, self._curfile, span)
        fname = match.group('fname')
        for incdir in [self._curdir] + self._includedirs:
            fpath = os.path.join(incdir, fname)
            if os.path.exists(fpath):
                break
        else:
            msg = "include file '{0}' not found".format(fname)
            raise FyppFatalError(msg, self._curfile, span)
        inpfp = _open_input_file(fpath, self._encoding)
        self._includefile(span, inpfp, fpath, os.path.dirname(fpath))
        inpfp.close()


    def _process_mute(self, span):
        if span[0] == span[1]:
            msg = 'Inline form of mute directive not allowed'
            raise FyppFatalError(msg, self._curfile, span)
        self.handle_mute(span)


    def _process_endmute(self, span):
        if span[0] == span[1]:
            msg = 'Inline form of endmute directive not allowed'
            raise FyppFatalError(msg, self._curfile, span)
        self.handle_endmute(span)


    def _check_param_presence(self, presence, directive, param, span):
        if (param is not None) != presence:
            if presence:
                msg = 'missing data in {0} directive'.format(directive)
            else:
                msg = 'forbidden data in {0} directive'.format(directive)
            raise FyppFatalError(msg, self._curfile, span)


    def _check_not_inline_directive(self, directive, span):
        if span[0] == span[1]:
            msg = 'Inline form of {0} directive not allowed'.format(directive)
            raise FyppFatalError(msg, self._curfile, span)


    @staticmethod
    def _unescape(txt):
        txt = _UNESCAPE_TEXT_REGEXP1.sub(r'\1\2\3', txt)
        txt = _UNESCAPE_TEXT_REGEXP2.sub(r'#\1\2', txt)
        txt = _UNESCAPE_TEXT_REGEXP3.sub(r'\1\2\3', txt)
        return txt


class Builder:
    '''Builds a tree representing a text with preprocessor directives.
    '''

    def __init__(self):
        # The tree, which should be built.
        self._tree = []

        # List of all open constructs
        self._open_blocks = []

        # Nodes to which the open blocks have to be appended when closed
        self._path = []

        # Nr. of open blocks when file was opened. Used for checking whether all
        # blocks have been closed, when file processing finishes.
        self._nr_prev_blocks = []

        # Current node, to which content should be added
        self._curnode = self._tree

        # Current file
        self._curfile = None


    def reset(self):
        '''Resets the builder so that it starts to build a new tree.'''
        self._tree = []
        self._open_blocks = []
        self._path = []
        self._nr_prev_blocks = []
        self._curnode = self._tree
        self._curfile = None


    def handle_include(self, span, fname):
        '''Should be called to signalize change to new file.

        Args:
            span (tuple of int): Start and end line of the include directive
                or None if called the first time for the main input.
            fname (str): Name of the file to be included.
        '''
        self._path.append(self._curnode)
        self._curnode = []
        self._open_blocks.append(
            ('include', self._curfile, [span], fname, None))
        self._curfile = fname
        self._nr_prev_blocks.append(len(self._open_blocks))


    def handle_endinclude(self, span, fname):
        '''Should be called when processing of a file finished.

        Args:
            span (tuple of int): Start and end line of the include directive
                or None if called the first time for the main input.
            fname (str): Name of the file which has been included.
        '''
        nprev_blocks = self._nr_prev_blocks.pop(-1)
        if len(self._open_blocks) > nprev_blocks:
            directive, fname, spans = self._open_blocks[-1][0:3]
            msg = '{0} directive still unclosed when reaching end of file'\
                  .format(directive)
            raise FyppFatalError(msg, self._curfile, spans[0])
        block = self._open_blocks.pop(-1)
        directive, blockfname, spans = block[0:3]
        if directive != 'include':
            msg = 'internal error: last open block is not \'include\' when '\
                  'closing file \'{0}\''.format(fname)
            raise FyppFatalError(msg)
        if span != spans[0]:
            msg = 'internal error: span for include and endinclude differ ('\
                  '{0} vs {1}'.format(span, spans[0])
            raise FyppFatalError(msg)
        oldfname, _ = block[3:5]
        if fname != oldfname:
            msg = 'internal error: mismatching file name in close_file event'\
                  " (expected: '{0}', got: '{1}')".format(oldfname, fname)
            raise FyppFatalError(msg, fname)
        block = directive, blockfname, spans, fname, self._curnode
        self._curnode = self._path.pop(-1)
        self._curnode.append(block)
        self._curfile = blockfname


    def handle_if(self, span, cond):
        '''Should be called to signalize an if directive.

        Args:
            span (tuple of int): Start and end line of the directive.
            param (str): String representation of the branching condition.
        '''
        self._path.append(self._curnode)
        self._curnode = []
        self._open_blocks.append(('if', self._curfile, [span], [cond], []))


    def handle_elif(self, span, cond):
        '''Should be called to signalize an elif directive.

        Args:
            span (tuple of int): Start and end line of the directive.
            cond (str): String representation of the branching condition.
        '''
        self._check_for_open_block(span, 'elif')
        block = self._open_blocks[-1]
        directive, _, spans = block[0:3]
        self._check_if_matches_last(directive, 'if', spans[-1], span, 'elif')
        conds, contents = block[3:5]
        conds.append(cond)
        contents.append(self._curnode)
        spans.append(span)
        self._curnode = []


    def handle_else(self, span):
        '''Should be called to signalize an else directive.

        Args:
            span (tuple of int): Start and end line of the directive.
        '''
        self._check_for_open_block(span, 'else')
        block = self._open_blocks[-1]
        directive, _, spans = block[0:3]
        self._check_if_matches_last(directive, 'if', spans[-1], span, 'else')
        conds, contents = block[3:5]
        conds.append('True')
        contents.append(self._curnode)
        spans.append(span)
        self._curnode = []


    def handle_endif(self, span):
        '''Should be called to signalize an endif directive.

        Args:
            span (tuple of int): Start and end line of the directive.
        '''
        self._check_for_open_block(span, 'endif')
        block = self._open_blocks.pop(-1)
        directive, _, spans = block[0:3]
        self._check_if_matches_last(directive, 'if', spans[-1], span, 'endif')
        _, contents = block[3:5]
        contents.append(self._curnode)
        spans.append(span)
        self._curnode = self._path.pop(-1)
        self._curnode.append(block)


    def handle_for(self, span, loopvar, iterator):
        '''Should be called to signalize a for directive.

        Args:
            span (tuple of int): Start and end line of the directive.
            varexpr (str): String representation of the loop variable
                expression.
            iterator (str): String representation of the iterable.
        '''
        self._path.append(self._curnode)
        self._curnode = []
        self._open_blocks.append(('for', self._curfile, [span], loopvar,
                                  iterator, None))


    def handle_endfor(self, span):
        '''Should be called to signalize an endfor directive.

        Args:
            span (tuple of int): Start and end line of the directive.
        '''
        self._check_for_open_block(span, 'endfor')
        block = self._open_blocks.pop(-1)
        directive, fname, spans = block[0:3]
        self._check_if_matches_last(directive, 'for', spans[-1], span, 'endfor')
        loopvar, iterator, dummy = block[3:6]
        spans.append(span)
        block = (directive, fname, spans, loopvar, iterator, self._curnode)
        self._curnode = self._path.pop(-1)
        self._curnode.append(block)


    def handle_def(self, span, name, argexpr):
        '''Should be called to signalize a def directive.

        Args:
            span (tuple of int): Start and end line of the directive.
            name (str): Name of the macro to be defined.
            argexpr (str): Macro argument definition or None
        '''
        self._path.append(self._curnode)
        self._curnode = []
        defblock = ('def', self._curfile, [span], name, argexpr, None)
        self._open_blocks.append(defblock)


    def handle_enddef(self, span, name):
        '''Should be called to signalize an enddef directive.

        Args:
            span (tuple of int): Start and end line of the directive.
            name (str): Name of the enddef statement. Could be None, if enddef
                was specified without name.
        '''
        self._check_for_open_block(span, 'enddef')
        block = self._open_blocks.pop(-1)
        directive, fname, spans = block[0:3]
        self._check_if_matches_last(directive, 'def', spans[-1], span, 'enddef')
        defname, argexpr, dummy = block[3:6]
        if name is not None and name != defname:
            msg = "wrong name in enddef directive "\
                  "(expected '{0}', got '{1}')".format(defname, name)
            raise FyppFatalError(msg, fname, span)
        spans.append(span)
        block = (directive, fname, spans, defname, argexpr, self._curnode)
        self._curnode = self._path.pop(-1)
        self._curnode.append(block)


    def handle_call(self, span, name, argexpr, blockcall):
        '''Should be called to signalize a call directive.

        Args:
            span (tuple of int): Start and end line of the directive.
            name (str): Name of the callable to call
            argexpr (str or None): Argument expression containing additional
                arguments for the call.
            blockcall (bool): Whether the alternative "block / contains /
                endblock" calling directive has been used.
        '''
        self._path.append(self._curnode)
        self._curnode = []
        directive = 'block' if blockcall else 'call'
        self._open_blocks.append(
            (directive, self._curfile, [span, span], name, argexpr, [], []))


    def handle_nextarg(self, span, name, blockcall):
        '''Should be called to signalize a nextarg directive.

        Args:
            span (tuple of int): Start and end line of the directive.
            name (str or None): Name of the argument following next or
                None if it should be the next positional argument.
            blockcall (bool): Whether the alternative "block / contains /
                endblock" calling directive has been used.
        '''
        self._check_for_open_block(span, 'nextarg')
        block = self._open_blocks[-1]
        directive, fname, spans = block[0:3]
        if blockcall:
            opened, current = 'block', 'contains'
        else:
            opened, current = 'call', 'nextarg'
        self._check_if_matches_last(directive, opened, spans[-1], span, current)
        args, argnames = block[5:7]
        args.append(self._curnode)
        spans.append(span)
        if name is not None:
            argnames.append(name)
        elif argnames:
            msg = 'non-keyword argument following keyword argument'
            raise FyppFatalError(msg, fname, span)
        self._curnode = []


    def handle_endcall(self, span, name, blockcall):
        '''Should be called to signalize an endcall directive.

        Args:
            span (tuple of int): Start and end line of the directive.
            name (str): Name of the endcall statement. Could be None, if endcall
                was specified without name.
            blockcall (bool): Whether the alternative "block / contains /
                endblock" calling directive has been used.
        '''
        self._check_for_open_block(span, 'endcall')
        block = self._open_blocks.pop(-1)
        directive, fname, spans = block[0:3]
        callname, callargexpr, args, argnames = block[3:7]
        if blockcall:
            opened, current = 'block', 'endblock'
        else:
            opened, current = 'call', 'endcall'
        self._check_if_matches_last(directive, opened, spans[0], span, current)

        if name is not None and name != callname:
            msg = "wrong name in {0} directive "\
                  "(expected '{1}', got '{2}')".format(current, callname, name)
            raise FyppFatalError(msg, fname, span)
        args.append(self._curnode)
        # If nextarg or endcall immediately followed call, then first argument
        # is empty and should be removed (to allow for calls without arguments
        # and named first argument in calls)
        if args and not args[0]:
            if len(argnames) == len(args):
                del argnames[0]
            del args[0]
            del spans[1]
        spans.append(span)
        block = (directive, fname, spans, callname, callargexpr, args, argnames)
        self._curnode = self._path.pop(-1)
        self._curnode.append(block)


    def handle_set(self, span, name, expr):
        '''Should be called to signalize a set directive.

        Args:
            span (tuple of int): Start and end line of the directive.
            name (str): Name of the variable.
            expr (str): String representation of the expression to be assigned
                to the variable.
        '''
        self._curnode.append(('set', self._curfile, span, name, expr))


    def handle_global(self, span, name):
        '''Should be called to signalize a global directive.

        Args:
            span (tuple of int): Start and end line of the directive.
            name (str): Name of the variable(s) to make global.
        '''
        self._curnode.append(('global', self._curfile, span, name))


    def handle_del(self, span, name):
        '''Should be called to signalize a del directive.

        Args:
            span (tuple of int): Start and end line of the directive.
            name (str): Name of the variable(s) to delete.
        '''
        self._curnode.append(('del', self._curfile, span, name))


    def handle_eval(self, span, expr):
        '''Should be called to signalize an eval directive.

        Args:
            span (tuple of int): Start and end line of the directive.
            expr (str): String representation of the Python expression to
                be evaluated.
        '''
        self._curnode.append(('eval', self._curfile, span, expr))


    def handle_comment(self, span):
        '''Should be called to signalize a comment directive.

        The content of the comment is not needed by the builder, but it needs
        the span of the comment to generate proper line numbers if needed.

        Args:
            span (tuple of int): Start and end line of the directive.
        '''
        self._curnode.append(('comment', self._curfile, span))


    def handle_text(self, span, txt):
        '''Should be called to pass text which goes to output unaltered.

        Args:
            span (tuple of int): Start and end line of the text.
            txt (str): Text.
        '''
        self._curnode.append(('txt', self._curfile, span, txt))


    def handle_mute(self, span):
        '''Should be called to signalize a mute directive.

        Args:
            span (tuple of int): Start and end line of the directive.
        '''
        self._path.append(self._curnode)
        self._curnode = []
        self._open_blocks.append(('mute', self._curfile, [span], None))


    def handle_endmute(self, span):
        '''Should be called to signalize an endmute directive.

        Args:
            span (tuple of int): Start and end line of the directive.
        '''
        self._check_for_open_block(span, 'endmute')
        block = self._open_blocks.pop(-1)
        directive, fname, spans = block[0:3]
        self._check_if_matches_last(directive, 'mute', spans[-1], span,
                                    'endmute')
        spans.append(span)
        block = (directive, fname, spans, self._curnode)
        self._curnode = self._path.pop(-1)
        self._curnode.append(block)


    def handle_stop(self, span, msg):
        '''Should be called to signalize a stop directive.

        Args:
            span (tuple of int): Start and end line of the directive.
        '''
        self._curnode.append(('stop', self._curfile, span, msg))


    def handle_assert(self, span, cond):
        '''Should be called to signalize an assert directive.

        Args:
            span (tuple of int): Start and end line of the directive.
        '''
        self._curnode.append(('assert', self._curfile, span, cond))


    @property
    def tree(self):
        '''Returns the tree built by the Builder.'''
        return self._tree


    def _check_for_open_block(self, span, directive):
        if len(self._open_blocks) <= self._nr_prev_blocks[-1]:
            msg = 'unexpected {0} directive'.format(directive)
            raise FyppFatalError(msg, self._curfile, span)


    def _check_if_matches_last(self, lastdir, curdir, lastspan, curspan,
                               directive):
        if curdir != lastdir:
            msg = "mismatching '{0}' directive (last block opened was '{1}')"\
                .format(directive, lastdir)
            raise FyppFatalError(msg, self._curfile, curspan)
        inline_last = lastspan[0] == lastspan[1]
        inline_cur = curspan[0] == curspan[1]
        if inline_last != inline_cur:
            if inline_cur:
                msg = 'expecting line form of directive {0}'.format(directive)
            else:
                msg = 'expecting inline form of directive {0}'.format(directive)
            raise FyppFatalError(msg, self._curfile, curspan)
        elif inline_cur and curspan[0] != lastspan[0]:
            msg = 'inline directives of the same construct must be in the '\
                  'same row'
            raise FyppFatalError(msg, self._curfile, curspan)


class Renderer:

    ''''Renders a tree.

    Args:
        evaluator (Evaluator, optional): Evaluator to use when rendering eval
            directives. If None (default), Evaluator() is used.
        linenums (bool, optional): Whether linenums should be generated,
            defaults to False.
        contlinenums (bool, optional): Whether linenums for continuation
            should be generated, defaults to False.
        linenumformat (str, optional): 'std', 'cpp' or 'gfortran5' depending
            what kind of line directives should be created. Default: 'cpp'.
            Format 'std' emits #line pragmas, 'cpp' resembles GNU cpps special
            format, and 'gfortran5' adds to cpp a workaround for a bug introduced in GFortran 5.
        linefolder (callable): Callable to use when folding a line.
        filevarroot (str, optional): render _FILE_ and _THIS_FILE_ as paths relative to this
            root directory (default: paths are not converted explicitely to relative paths)
    '''

    def __init__(self, evaluator=None, linenums=False, contlinenums=False,
                 linenumformat=None, linefolder=None, filevarroot=None):
        # Evaluator to use for Python expressions
        self._evaluator = Evaluator() if evaluator is None else evaluator
        self._evaluator.updateglobals(_SYSTEM_=platform.system(),
            _MACHINE_=platform.machine())

        # Whether rendered output is diverted and will be processed
        # further before output (if True: no line numbering and post processing)
        self._diverted = False

        # Whether file name and line numbers should be kept fixed and
        # not updated (typically when rendering macro content)
        self._fixedposition = False

        # Whether line numbering directives should be emitted
        self._linenums = linenums

        # Whether line numbering directives in continuation lines are needed.
        self._contlinenums = contlinenums

        # Line number formatter function and whether gfortran5 fix is needed
        if linenumformat is None or linenumformat in ('cpp', 'gfortran5'):
            self._linenumdir = linenumdir_cpp
            self._linenum_gfortran5 = linenumformat == 'gfortran5'
        else:
            self._linenumdir = linenumdir_std
            self._linenum_gfortran5 = False

        # Callable to be used for folding lines
        if linefolder is None:
            self._linefolder = lambda line: [line]
        else:
            self._linefolder = linefolder

        if filevarroot is None:
            self._convert_file_path = lambda path: path
        else:
            self._convert_file_path = (
                lambda path: pathlib.Path(path).relative_to(filevarroot)
            )


    def render(self, tree, divert=False, fixposition=False):
        '''Renders a tree.

        Args:
            tree (fypp-tree): Tree to render.
            divert (bool): Whether output will be diverted and sent for further
                processing, so that no line numbering directives and
                postprocessing are needed at this stage. (Default: False)
            fixposition (bool): Whether file name and line position (variables
                _FILE_ and _LINE_) should be kept at their current values or
                should be updated continuously. (Default: False).

        Returns: str: Rendered string.
        '''
        diverted = self._diverted
        self._diverted = divert
        fixedposition_old = self._fixedposition
        self._fixedposition = self._fixedposition or fixposition
        output, eval_inds, eval_pos = self._render(tree)
        if not self._diverted and eval_inds:
            self._postprocess_eval_lines(output, eval_inds, eval_pos)
        self._diverted = diverted
        self._fixedposition = fixedposition_old
        txt = ''.join(output)

        return txt


    def _render(self, tree):
        output = []
        eval_inds = []
        eval_pos = []
        for node in tree:
            cmd = node[0]
            if cmd == 'txt':
                output.append(node[3])
            elif cmd == 'if':
                out, ieval, peval = self._get_conditional_content(*node[1:5])
                eval_inds += _shiftinds(ieval, len(output))
                eval_pos += peval
                output += out
            elif cmd == 'eval':
                out, ieval, peval = self._get_eval(*node[1:4])
                eval_inds += _shiftinds(ieval, len(output))
                eval_pos += peval
                output += out
            elif cmd == 'def':
                result = self._define_macro(*node[1:6])
                output.append(result)
            elif cmd == 'set':
                result = self._define_variable(*node[1:5])
                output.append(result)
            elif cmd == 'del':
                self._delete_variable(*node[1:4])
            elif cmd == 'for':
                out, ieval, peval = self._get_iterated_content(*node[1:6])
                eval_inds += _shiftinds(ieval, len(output))
                eval_pos += peval
                output += out
            elif cmd == 'call' or cmd == 'block':
                out, ieval, peval = self._get_called_content(*node[1:7])
                eval_inds += _shiftinds(ieval, len(output))
                eval_pos += peval
                output += out
            elif cmd == 'include':
                out, ieval, peval = self._get_included_content(*node[1:5])
                eval_inds += _shiftinds(ieval, len(output))
                eval_pos += peval
                output += out
            elif cmd == 'comment':
                output.append(self._get_comment(*node[1:3]))
            elif cmd == 'mute':
                output.append(self._get_muted_content(*node[1:4]))
            elif cmd == 'stop':
                self._handle_stop(*node[1:4])
            elif cmd == 'assert':
                result = self._handle_assert(*node[1:4])
                output.append(result)
            elif cmd == 'global':
                self._add_global(*node[1:4])
            else:
                msg = "internal error: unknown command '{0}'".format(cmd)
                raise FyppFatalError(msg)
        return output, eval_inds, eval_pos


    def _get_eval(self, fname, span, expr):
        try:
            result = self._evaluate(expr, fname, span[0])
        except Exception as exc:
            msg = "exception occurred when evaluating '{0}'".format(expr)
            raise FyppFatalError(msg, fname, span) from exc
        out = []
        ieval = []
        peval = []
        if result is not None:
            out.append(str(result))
            if not self._diverted:
                ieval.append(0)
                peval.append((span, fname))
        if span[0] != span[1]:
            out.append('\n')
        return out, ieval, peval


    def _get_conditional_content(self, fname, spans, conditions, contents):
        out = []
        ieval = []
        peval = []
        multiline = (spans[0][0] != spans[-1][1])
        for condition, content, span in zip(conditions, contents, spans):
            try:
                cond = bool(self._evaluate(condition, fname, span[0]))
            except Exception as exc:
                msg = "exception occurred when evaluating '{0}'"\
                      .format(condition)
                raise FyppFatalError(msg, fname, span) from exc
            if cond:
                if self._linenums and not self._diverted and multiline:
                    out.append(self._linenumdir(span[1], fname))
                outcont, ievalcont, pevalcont = self._render(content)
                ieval += _shiftinds(ievalcont, len(out))
                peval += pevalcont
                out += outcont
                break
        if self._linenums and not self._diverted and multiline:
            out.append(self._linenumdir(spans[-1][1], fname))
        return out, ieval, peval


    def _get_iterated_content(self, fname, spans, loopvars, loopiter, content):
        out = []
        ieval = []
        peval = []
        try:
            iterobj = iter(self._evaluate(loopiter, fname, spans[0][0]))
        except Exception as exc:
            msg = "exception occurred when evaluating '{0}'"\
                .format(loopiter)
            raise FyppFatalError(msg, fname, spans[0]) from exc
        multiline = (spans[0][0] != spans[-1][1])
        for var in iterobj:
            if len(loopvars) == 1:
                self._define(loopvars[0], var)
            else:
                for varname, value in zip(loopvars, var):
                    self._define(varname, value)
            if self._linenums and not self._diverted and multiline:
                out.append(self._linenumdir(spans[0][1], fname))
            outcont, ievalcont, pevalcont = self._render(content)
            ieval += _shiftinds(ievalcont, len(out))
            peval += pevalcont
            out += outcont
        if self._linenums and not self._diverted and multiline:
            out.append(self._linenumdir(spans[1][1], fname))
        return out, ieval, peval


    def _get_called_content(self, fname, spans, name, argexpr, contents,
                            argnames):
        posargs, kwargs = self._get_call_arguments(fname, spans, argexpr,
                                                   contents, argnames)
        try:
            callobj = self._evaluate(name, fname, spans[0][0])
            result = callobj(*posargs, **kwargs)
        except Exception as exc:
            msg = "exception occurred when calling '{0}'".format(name)
            raise FyppFatalError(msg, fname, spans[0]) from exc
        self._update_predef_globals(fname, spans[0][0])
        span = (spans[0][0], spans[-1][1])
        out = []
        ieval = []
        peval = []
        if result is not None:
            out = [str(result)]
            if not self._diverted:
                ieval = [0]
                peval = [(span, fname)]
        if span[0] != span[1]:
            out.append('\n')
        return out, ieval, peval


    def _get_call_arguments(self, fname, spans, argexpr, contents, argnames):
        if argexpr is None:
            posargs = []
            kwargs = {}
        else:
            # Parse and evaluate arguments passed in call header
            self._evaluator.openscope()
            try:
                posargs, kwargs = self._evaluate(
                    '__getargvalues(' + argexpr + ')', fname, spans[0][0])
            except Exception as exc:
                msg = "unable to parse argument expression '{0}'"\
                    .format(argexpr)
                raise FyppFatalError(msg, fname, spans[0]) from exc
            self._evaluator.closescope()

        # Render arguments passed in call body
        args = []
        for content in contents:
            self._evaluator.openscope()
            rendered = self.render(content, divert=True)
            self._evaluator.closescope()
            if rendered.endswith('\n'):
                rendered = rendered[:-1]
            args.append(rendered)

        # Separate arguments in call body into positional and keyword ones:
        if argnames:
            posargs += args[:len(args) - len(argnames)]
            offset = len(args) - len(argnames)
            for iargname, argname in enumerate(argnames):
                ind = offset + iargname
                if argname in kwargs:
                    msg = "keyword argument '{0}' already defined"\
                        .format(argname)
                    raise FyppFatalError(msg, fname, spans[ind + 1])
                kwargs[argname] = args[ind]
        else:
            posargs += args

        return posargs, kwargs


    def _get_included_content(self, fname, spans, includefname, content):
        includefile = spans[0] is not None
        out = []
        if self._linenums and not self._diverted:
            if includefile or self._linenum_gfortran5:
                out += self._linenumdir(0, includefname, _LINENUM_NEW_FILE)
            else:
                out += self._linenumdir(0, includefname)
        outcont, ieval, peval = self._render(content)
        ieval = _shiftinds(ieval, len(out))
        out += outcont
        if self._linenums and not self._diverted and includefile:
            out += self._linenumdir(spans[0][1], fname, _LINENUM_RETURN_TO_FILE)
        return out, ieval, peval


    def _define_macro(self, fname, spans, name, argexpr, content):
        if argexpr is None:
            args = []
            defaults = {}
            varpos = None
            varkw = None
        else:
            # Try to create a lambda function with the argument expression
            self._evaluator.openscope()
            lambdaexpr = 'lambda ' + argexpr + ': None'
            try:
                func = self._evaluate(lambdaexpr, fname, spans[0][0])
            except Exception as exc:
                msg = "exception occurred when evaluating argument expression "\
                      "'{0}'".format(argexpr)
                raise FyppFatalError(msg, fname, spans[0]) from exc
            self._evaluator.closescope()
            try:
                args, defaults, varpos, varkw = _get_callable_argspec(func)
            except Exception as exc:
                msg = "invalid argument expression '{0}'".format(argexpr)
                raise FyppFatalError(msg, fname, spans[0]) from exc
            named_args = args if varpos is None else args + [varpos]
            named_args = named_args if varkw is None else named_args + [varkw]
            for arg in named_args:
                if arg in _RESERVED_NAMES or arg.startswith(_RESERVED_PREFIX):
                    msg = "invalid argument name '{0}'".format(arg)
                    raise FyppFatalError(msg, fname, spans[0])
        result = ''
        try:
            macro = _Macro(
                name, fname, spans, args, defaults, varpos, varkw, content,
                self, self._evaluator, self._evaluator.localscope)
            self._define(name, macro)
        except Exception as exc:
            msg = "exception occurred when defining macro '{0}'"\
                .format(name)
            raise FyppFatalError(msg, fname, spans[0]) from exc
        if self._linenums and not self._diverted:
            result = self._linenumdir(spans[1][1], fname)
        return result


    def _define_variable(self, fname, span, name, valstr):
        result = ''
        try:
            if valstr is None:
                expr = None
            else:
                expr = self._evaluate(valstr, fname, span[0])
            self._define(name, expr)
        except Exception as exc:
            msg = "exception occurred when setting variable(s) '{0}' to '{1}'"\
                .format(name, valstr)
            raise FyppFatalError(msg, fname, span) from exc
        multiline = (span[0] != span[1])
        if self._linenums and not self._diverted and multiline:
            result = self._linenumdir(span[1], fname)
        return result


    def _delete_variable(self, fname, span, name):
        result = ''
        try:
            self._evaluator.undefine(name)
        except Exception as exc:
            msg = "exception occurred when deleting variable(s) '{0}'"\
                  .format(name)
            raise FyppFatalError(msg, fname, span) from exc
        multiline = (span[0] != span[1])
        if self._linenums and not self._diverted and multiline:
            result = self._linenumdir(span[1], fname)
        return result


    def _add_global(self, fname, span, name):
        result = ''
        try:
            self._evaluator.addglobal(name)
        except Exception as exc:
            msg = "exception occurred when making variable(s) '{0}' global"\
                .format(name)
            raise FyppFatalError(msg, fname, span) from exc
        multiline = (span[0] != span[1])
        if self._linenums and not self._diverted and multiline:
            result = self._linenumdir(span[1], fname)
        return result


    def _get_comment(self, fname, span):
        if self._linenums and not self._diverted:
            return self._linenumdir(span[1], fname)
        return ''


    def _get_muted_content(self, fname, spans, content):
        self._render(content)
        if self._linenums and not self._diverted:
            return self._linenumdir(spans[-1][1], fname)
        return ''


    def _handle_stop(self, fname, span, msgstr):
        try:
            msg = str(self._evaluate(msgstr, fname, span[0]))
        except Exception as exc:
            msg = "exception occurred when evaluating stop message '{0}'"\
                .format(msgstr)
            raise FyppFatalError(msg, fname, span) from exc
        raise FyppStopRequest(msg, fname, span)


    def _handle_assert(self, fname, span, expr):
        result = ''
        try:
            cond = bool(self._evaluate(expr, fname, span[0]))
        except Exception as exc:
            msg = "exception occurred when evaluating assert condition '{0}'"\
                .format(expr)
            raise FyppFatalError(msg, fname, span) from exc
        if not cond:
            msg = "Assertion failed ('{0}')".format(expr)
            raise FyppStopRequest(msg, fname, span)
        if self._linenums and not self._diverted:
            result = self._linenumdir(span[1], fname)
        return result


    def _evaluate(self, expr, fname, linenr):
        self._update_predef_globals(fname, linenr)
        result = self._evaluator.evaluate(expr)
        self._update_predef_globals(fname, linenr)
        return result


    def _update_predef_globals(self, fname, linenr):
        fname = self._convert_file_path(fname)
        self._evaluator.updatelocals(
            _DATE_=time.strftime('%Y-%m-%d'), _TIME_=time.strftime('%H:%M:%S'),
            _THIS_FILE_=fname, _THIS_LINE_=linenr + 1)
        if not self._fixedposition:
            self._evaluator.updateglobals(_FILE_=fname, _LINE_=linenr + 1)


    def _define(self, var, value):
        self._evaluator.define(var, value)


    def _postprocess_eval_lines(self, output, eval_inds, eval_pos):
        ilastproc = -1
        for ieval, ind in enumerate(eval_inds):
            span, fname = eval_pos[ieval]
            if ind <= ilastproc:
                continue
            iprev, eolprev = self._find_last_eol(output, ind)
            inext, eolnext = self._find_next_eol(output, ind)
            curline = self._glue_line(output, ind, iprev, eolprev, inext,
                                      eolnext)
            output[iprev + 1:inext] = [''] * (inext - iprev - 1)
            output[ind] = self._postprocess_eval_line(curline, fname, span)
            ilastproc = inext


    @staticmethod
    def _find_last_eol(output, ind):
        'Find last newline before current position.'
        iprev = ind - 1
        while iprev >= 0:
            eolprev = output[iprev].rfind('\n')
            if eolprev != -1:
                break
            iprev -= 1
        else:
            iprev = 0
            eolprev = -1
        return iprev, eolprev


    @staticmethod
    def _find_next_eol(output, ind):
        'Find last newline before current position.'
        # find first eol after expr. evaluation
        inext = ind + 1
        while inext < len(output):
            eolnext = output[inext].find('\n')
            if eolnext != -1:
                break
            inext += 1
        else:
            inext = len(output) - 1
            eolnext = len(output[-1]) - 1
        return inext, eolnext


    @staticmethod
    def _glue_line(output, ind, iprev, eolprev, inext, eolnext):
        'Create line from parts between specified boundaries.'
        curline_parts = []
        if iprev != ind:
            curline_parts = [output[iprev][eolprev + 1:]]
            output[iprev] = output[iprev][:eolprev + 1]
        curline_parts.extend(output[iprev + 1:ind])
        curline_parts.extend(output[ind])
        curline_parts.extend(output[ind + 1:inext])
        if inext != ind:
            curline_parts.append(output[inext][:eolnext + 1])
            output[inext] = output[inext][eolnext + 1:]
        return ''.join(curline_parts)


    def _postprocess_eval_line(self, evalline, fname, span):
        lines = evalline.split('\n')
        # If line ended on '\n', last element is ''. We remove it and
        # add the trailing newline later manually.
        trailing_newline = (lines[-1] == '')
        if trailing_newline:
            del lines[-1]
        lnum = self._linenumdir(span[0], fname) if self._linenums else ''
        clnum = lnum if self._contlinenums else ''
        linenumsep = '\n' + lnum
        clinenumsep = '\n' + clnum
        foldedlines = [self._foldline(line) for line in lines]
        outlines = [clinenumsep.join(lines) for lines in foldedlines]
        result = linenumsep.join(outlines)
        # Add missing trailing newline
        if trailing_newline:
            trailing = '\n'
            if self._linenums:
                # Last line was folded, but no linenums were generated for
                # the continuation lines -> current line position is not
                # in sync with the one calculated from the last line number
                unsync = (
                    len(foldedlines) and len(foldedlines[-1]) > 1
                    and not self._contlinenums)
                # Eval directive in source consists of more than one line
                multiline = span[1] - span[0] > 1
                if unsync or multiline:
                    # For inline eval directives span[0] == span[1]
                    # -> next line is span[0] + 1 and not span[1] as for
                    # line eval directives
                    nextline = max(span[1], span[0] + 1)
                    trailing += self._linenumdir(nextline, fname)
        else:
            trailing = ''
        return result + trailing


    def _foldline(self, line):
        if _COMMENTLINE_REGEXP.match(line) is None:
            return self._linefolder(line)
        return [line]


class Evaluator:

    '''Provides an isolated environment for evaluating Python expressions.

    It restricts the builtins which can be used within this environment to a
    (hopefully safe) subset. Additionally it defines the functions which are
    provided by the preprocessor for the eval directives.

    Args:
        env (dict, optional): Initial definitions for the environment, defaults
            to None.
    '''

    # Restricted builtins working in all supported Python verions. Version
    # specific ones are added dynamically in _get_restricted_builtins().
    _RESTRICTED_BUILTINS = {
        'abs': builtins.abs,
        'all': builtins.all,
        'any': builtins.any,
        'bin': builtins.bin,
        'bool': builtins.bool,
        'bytearray': builtins.bytearray,
        'bytes': builtins.bytes,
        'chr': builtins.chr,
        'classmethod': builtins.classmethod,
        'complex': builtins.complex,
        'delattr': builtins.delattr,
        'dict': builtins.dict,
        'dir': builtins.dir,
        'divmod': builtins.divmod,
        'enumerate': builtins.enumerate,
        'filter': builtins.filter,
        'float': builtins.float,
        'format': builtins.format,
        'frozenset': builtins.frozenset,
        'getattr': builtins.getattr,
        'globals': builtins.globals,
        'hasattr': builtins.hasattr,
        'hash': builtins.hash,
        'hex': builtins.hex,
        'id': builtins.id,
        'int': builtins.int,
        'isinstance': builtins.isinstance,
        'issubclass': builtins.issubclass,
        'iter': builtins.iter,
        'len': builtins.len,
        'list': builtins.list,
        'locals': builtins.locals,
        'map': builtins.map,
        'max': builtins.max,
        'min': builtins.min,
        'next': builtins.next,
        'object': builtins.object,
        'oct': builtins.oct,
        'ord': builtins.ord,
        'pow': builtins.pow,
        'property': builtins.property,
        'range': builtins.range,
        'repr': builtins.repr,
        'reversed': builtins.reversed,
        'round': builtins.round,
        'set': builtins.set,
        'setattr': builtins.setattr,
        'slice': builtins.slice,
        'sorted': builtins.sorted,
        'staticmethod': builtins.staticmethod,
        'str': builtins.str,
        'sum': builtins.sum,
        'super': builtins.super,
        'tuple': builtins.tuple,
        'type': builtins.type,
        'vars': builtins.vars,
        'zip': builtins.zip,
    }


    def __init__(self, env=None):

        # Global scope
        self._globals = env if env is not None else {}

        # Local scope(s)
        self._locals = None
        self._locals_stack = []

        # Variables which are references to entries in global scope
        self._globalrefs = None
        self._globalrefs_stack = []

        # Current scope (globals + locals in all embedding and in current scope)
        self._scope = self._globals

        # Turn on restricted mode
        self._restrict_builtins()


    def evaluate(self, expr):
        '''Evaluate a Python expression using the `eval()` builtin.

        Args:
            expr (str): String represantion of the expression.

        Return:
            Python object: Result of the expression evaluation.
        '''
        result = eval(expr, self._scope)
        return result


    def import_module(self, module):
        '''Import a module into the evaluator.

        Note: Import only trustworthy modules! Module imports are global,
        therefore, importing a malicious module which manipulates other global
        modules could affect code behaviour outside of the Evaluator as well.

        Args:
            module (str): Python module to import.

        Raises:
            FyppFatalError: If module could not be imported.

        '''
        rootmod = module.split('.', 1)[0]
        try:
            imported = __import__(module, self._scope)
            self.define(rootmod, imported)
        except Exception as exc:
            msg = "failed to import module '{0}'".format(module)
            raise FyppFatalError(msg) from exc


    def define(self, name, value):
        '''Define a Python entity.

        Args:
            name (str): Name of the entity.
            value (Python object): Value of the entity.

        Raises:
            FyppFatalError: If name starts with the reserved prefix or if it is
                a reserved name.
        '''
        varnames = self._get_variable_names(name)
        if len(varnames) == 1:
            value = (value,)
        elif len(varnames) != len(value):
            msg = 'value for tuple assignment has incompatible length'
            raise FyppFatalError(msg)
        for varname, varvalue in zip(varnames, value):
            self._check_variable_name(varname)
            if self._locals is None:
                self._globals[varname] = varvalue
            else:
                if varname in self._globalrefs:
                    self._globals[varname] = varvalue
                else:
                    self._locals[varname] = varvalue
                self._scope[varname] = varvalue


    def undefine(self, name):
        '''Undefine a Python entity.

        Args:
            name (str): Name of the entity to undefine.

        Raises:
            FyppFatalError: If name starts with the reserved prefix or if it is
                a reserved name.
        '''
        varnames = self._get_variable_names(name)
        for varname in varnames:
            self._check_variable_name(varname)
            deleted = False
            if self._locals is None:
                if varname in self._globals:
                    del self._globals[varname]
                    deleted = True
            else:
                if varname in self._locals:
                    del self._locals[varname]
                    del self._scope[varname]
                    deleted = True
                elif varname in self._globalrefs and varname in self._globals:
                    del self._globals[varname]
                    del self._scope[varname]
                    deleted = True
            if not deleted:
                msg = "lookup for an erasable instance of '{0}' failed"\
                      .format(varname)
                raise FyppFatalError(msg)


    def addglobal(self, name):
        '''Define a given entity as global.

        Args:
            name (str): Name of the entity to make global.

        Raises:
            FyppFatalError: If entity name is invalid or if the current scope is
                 a local scope and entity is already defined in it.
        '''
        varnames = self._get_variable_names(name)
        for varname in varnames:
            self._check_variable_name(varname)
            if self._locals is not None:
                if varname in self._locals:
                    msg = "variable '{0}' already defined in local scope"\
                          .format(varname)
                    raise FyppFatalError(msg)
                self._globalrefs.add(varname)


    def updateglobals(self, **vardict):
        '''Update variables in the global scope.

        This is a shortcut function to inject protected variables in the global
        scope without extensive checks (as in define()). Vardict must not
        contain any global entries which can be shadowed in local scopes
        (e.g. should only contain variables with forbidden prefix).

        Args:
            **vardict: variable definitions.

        '''
        self._scope.update(vardict)
        if self._locals is not None:
            self._globals.update(vardict)


    def updatelocals(self, **vardict):
        '''Update variables in the local scope.

        This is a shortcut function to inject variables in the local scope
        without extensive checks (as in define()). Vardict must not contain any
        entries which have been made global via addglobal() before. In order to
        ensure this, updatelocals() should be called immediately after
        openscope(), or with variable names, which are warrantedly not globals
        (e.g variables starting with forbidden prefix)

        Args:
            **vardict: variable definitions.
        '''
        self._scope.update(vardict)
        if self._locals is not None:
            self._locals.update(vardict)


    def openscope(self, customlocals=None):
        '''Opens a new (embedded) scope.

        Args:
            customlocals (dict): By default, the locals of the embedding scope
                are visible in the new one. When this is not the desired
                behaviour a dictionary of customized locals can be passed,
                and those locals will become the only visible ones.
        '''
        self._locals_stack.append(self._locals)
        self._globalrefs_stack.append(self._globalrefs)
        if customlocals is not None:
            self._locals = customlocals.copy()
        elif self._locals is not None:
            self._locals = self._locals.copy()
        else:
            self._locals = {}
        self._globalrefs = set()
        self._scope = self._globals.copy()
        self._scope.update(self._locals)


    def closescope(self):
        '''Close scope and restore embedding scope.'''
        self._locals = self._locals_stack.pop(-1)
        self._globalrefs = self._globalrefs_stack.pop(-1)
        if self._locals is not None:
            self._scope = self._globals.copy()
            self._scope.update(self._locals)
        else:
            self._scope = self._globals


    @property
    def globalscope(self):
        'Dictionary of the global scope.'
        return self._globals


    @property
    def localscope(self):
        'Dictionary of the current local scope.'
        return self._locals


    def _restrict_builtins(self):
        builtindict = self._get_restricted_builtins()
        builtindict['__import__'] = self._func_import
        builtindict['defined'] = self._func_defined
        builtindict['setvar'] = self._func_setvar
        builtindict['getvar'] = self._func_getvar
        builtindict['delvar'] = self._func_delvar
        builtindict['globalvar'] = self._func_globalvar
        builtindict['__getargvalues'] = self._func_getargvalues
        self._globals['__builtins__'] = builtindict


    @classmethod
    def _get_restricted_builtins(cls):
        bidict = dict(cls._RESTRICTED_BUILTINS)
        return bidict


    @staticmethod
    def _get_variable_names(varexpr):
        lpar = varexpr.startswith('(')
        rpar = varexpr.endswith(')')
        if lpar != rpar:
            msg = "unbalanced parenthesis around variable varexpr(s) in '{0}'"\
                .format(varexpr)
            raise FyppFatalError(msg, None, None)
        if lpar:
            varexpr = varexpr[1:-1]
        varnames = [s.strip() for s in varexpr.split(',')]
        return varnames


    @staticmethod
    def _check_variable_name(varname):
        if varname.startswith(_RESERVED_PREFIX):
            msg = "Name '{0}' starts with reserved prefix '{1}'"\
                .format(varname, _RESERVED_PREFIX)
            raise FyppFatalError(msg, None, None)
        if varname in _RESERVED_NAMES:
            msg = "Name '{0}' is reserved and can not be redefined"\
                .format(varname)
            raise FyppFatalError(msg, None, None)


    def _func_defined(self, var):
        defined = var in self._scope
        return defined


    def _func_import(self, name, *_, **__):
        module = self._scope.get(name, None)
        if module is not None and isinstance(module, types.ModuleType):
            return module
        msg = "Import of module '{0}' via '__import__' not allowed".format(name)
        raise ImportError(msg)


    def _func_setvar(self, *namesvalues):
        if len(namesvalues) % 2:
            msg = 'setvar function needs an even number of arguments'
            raise FyppFatalError(msg)
        for ind in range(0, len(namesvalues), 2):
            self.define(namesvalues[ind], namesvalues[ind + 1])


    def _func_getvar(self, name, defvalue=None):
        if name in self._scope:
            return self._scope[name]
        return defvalue


    def _func_delvar(self, *names):
        for name in names:
            self.undefine(name)


    def _func_globalvar(self, *names):
        for name in names:
            self.addglobal(name)


    @staticmethod
    def _func_getargvalues(*args, **kwargs):
        return list(args), kwargs



class _Macro:

    '''Represents a user defined macro.

    This object should only be initiatied by a Renderer instance, as it
    needs access to Renderers internal variables and methods.

    Args:
        name (str): Name of the macro.
        fname (str): The file where the macro was defined.
        spans (str): Line spans of macro definition.
        argnames (list of str): Macro dummy arguments.
        varpos (str): Name of variable positional argument or None.
        varkw (str): Name of variable keyword argument or None.
        content (list): Content of the macro as tree.
        renderer (Renderer): Renderer to use for evaluating macro content.
        localscope (dict): Dictionary with local variables, which should be used
            the local scope, when the macro is called. Default: None (empty
            local scope).
    '''

    def __init__(self, name, fname, spans, argnames, defaults, varpos, varkw,
                 content, renderer, evaluator, localscope=None):
        self._name = name
        self._fname = fname
        self._spans = spans
        self._argnames = argnames
        self._defaults = defaults
        self._varpos = varpos
        self._varkw = varkw
        self._content = content
        self._renderer = renderer
        self._evaluator = evaluator
        self._localscope = localscope if localscope is not None else {}


    def __call__(self, *args, **keywords):
        argdict = self._process_arguments(args, keywords)
        self._evaluator.openscope(customlocals=self._localscope)
        self._evaluator.updatelocals(**argdict)
        output = self._renderer.render(self._content, divert=True,
                                       fixposition=True)
        self._evaluator.closescope()
        if output.endswith('\n'):
            return output[:-1]
        return output


    def _process_arguments(self, args, keywords):
        kwdict = dict(keywords)
        argdict = {}
        nargs = min(len(args), len(self._argnames))
        for iarg in range(nargs):
            argdict[self._argnames[iarg]] = args[iarg]
        if nargs < len(args):
            if self._varpos is None:
                msg = "macro '{0}' called with too many positional arguments "\
                      "(expected: {1}, received: {2})"\
                      .format(self._name, len(self._argnames), len(args))
                raise FyppFatalError(msg, self._fname, self._spans[0])
            else:
                argdict[self._varpos] = list(args[nargs:])
        elif self._varpos is not None:
            argdict[self._varpos] = []
        for argname in self._argnames[:nargs]:
            if argname in kwdict:
                msg = "got multiple values for argument '{0}'".format(argname)
                raise FyppFatalError(msg, self._fname, self._spans[0])
        if nargs < len(self._argnames):
            for argname in self._argnames[nargs:]:
                if argname in kwdict:
                    argdict[argname] = kwdict.pop(argname)
                elif argname in self._defaults:
                    argdict[argname] = self._defaults[argname]
                else:
                    msg = "macro '{0}' called without mandatory positional "\
                          "argument '{1}'".format(self._name, argname)
                    raise FyppFatalError(msg, self._fname, self._spans[0])
        if kwdict and self._varkw is None:
            kwstr = "', '".join(kwdict.keys())
            msg = "macro '{0}' called with unknown keyword argument(s) '{1}'"\
                  .format(self._name, kwstr)
            raise FyppFatalError(msg, self._fname, self._spans[0])
        if self._varkw is not None:
            argdict[self._varkw] = kwdict
        return argdict



class Processor:

    '''Connects various objects with each other to create a processor.

    Args:
        parser (Parser, optional): Parser to use for parsing text. If None
            (default), `Parser()` is used.
        builder (Builder, optional): Builder to use for building the tree
            representation of the text. If None (default), `Builder()` is used.
        renderer (Renderer, optional): Renderer to use for rendering the
            output. If None (default), `Renderer()` is used with a default
            Evaluator().
        evaluator (Evaluator, optional): Evaluator to use for evaluating Python
            expressions. If None (default), `Evaluator()` is used.
    '''

    def __init__(self, parser=None, builder=None, renderer=None,
                 evaluator=None):
        self._parser = Parser() if parser is None else parser
        self._builder = Builder() if builder is None else builder
        if renderer is None:
            evaluator = Evaluator() if evaluator is None else evaluator
            self._renderer = Renderer(evaluator)
        else:
            self._renderer = renderer

        self._parser.handle_include = self._builder.handle_include
        self._parser.handle_endinclude = self._builder.handle_endinclude
        self._parser.handle_if = self._builder.handle_if
        self._parser.handle_else = self._builder.handle_else
        self._parser.handle_elif = self._builder.handle_elif
        self._parser.handle_endif = self._builder.handle_endif
        self._parser.handle_eval = self._builder.handle_eval
        self._parser.handle_text = self._builder.handle_text
        self._parser.handle_def = self._builder.handle_def
        self._parser.handle_enddef = self._builder.handle_enddef
        self._parser.handle_set = self._builder.handle_set
        self._parser.handle_del = self._builder.handle_del
        self._parser.handle_global = self._builder.handle_global
        self._parser.handle_for = self._builder.handle_for
        self._parser.handle_endfor = self._builder.handle_endfor
        self._parser.handle_call = self._builder.handle_call
        self._parser.handle_nextarg = self._builder.handle_nextarg
        self._parser.handle_endcall = self._builder.handle_endcall
        self._parser.handle_comment = self._builder.handle_comment
        self._parser.handle_mute = self._builder.handle_mute
        self._parser.handle_endmute = self._builder.handle_endmute
        self._parser.handle_stop = self._builder.handle_stop
        self._parser.handle_assert = self._builder.handle_assert


    def process_file(self, fname):
        '''Processeses a file.

        Args:
            fname (str): Name of the file to process.

        Returns:
            str: Processed content.
        '''
        self._parser.parsefile(fname)
        return self._render()


    def process_text(self, txt):
        '''Processes a string.

        Args:
            txt (str): Text to process.

        Returns:
            str: Processed content.
        '''
        self._parser.parse(txt)
        return self._render()


    def _render(self):
        output = self._renderer.render(self._builder.tree)
        self._builder.reset()
        return ''.join(output)


class Fypp:

    '''Fypp preprocessor.

    You can invoke it like ::

        tool = fypp.Fypp()
        tool.process_file('file.in', 'file.out')

    to initialize Fypp with default options, process `file.in` and write the
    result to `file.out`. If the input should be read from a string, the
    ``process_text()`` method can be used::

        tool = fypp.Fypp()
        output = tool.process_text('#:if DEBUG > 0\\nprint *, "DEBUG"\\n#:endif\\n')

    If you want to fine tune Fypps behaviour, pass a customized `FyppOptions`_
    instance at initialization::

        options = fypp.FyppOptions()
        options.fixed_format = True
        tool = fypp.Fypp(options)

    Alternatively, you can use the command line parser ``optparse.OptionParser``
    to set options for Fypp. The function ``get_option_parser()`` returns you a
    default option parser. You can then use its ``parse_args()`` method to
    obtain settings by reading the command line arguments::

        optparser = fypp.get_option_parser()
        options, leftover = optparser.parse_args()
        tool = fypp.Fypp(options)

    The command line options can also be passed directly as a list when
    calling ``parse_args()``::

        args = ['-DDEBUG=0', 'input.fpp', 'output.f90']
        optparser = fypp.get_option_parser()
        options, leftover = optparser.parse_args(args=args)
        tool = fypp.Fypp(options)

    For even more fine-grained control over how Fypp works, you can pass in
    custom factory methods that handle construction of the evaluator, parser,
    builder and renderer components. These factory methods must have the same
    signature as the corresponding component's constructor. As an example of
    using a builder that's customized by subclassing::

        class MyBuilder(fypp.Builder):

            def __init__(self):
               super().__init__()
               ...additional initialization...

        tool = fypp.Fypp(options, builder_factory=MyBuilder)


    Args:
        options (object): Object containing the settings for Fypp. You typically
            would pass a customized `FyppOptions`_ instance or an
            ``optparse.Values`` object as returned by the option parser. If not
            present, the default settings in `FyppOptions`_ are used.
        evaluator_factory (function): Factory function that returns an Evaluator
            object. Its call signature must match that of the Evaluator
            constructor. If not present, ``Evaluator`` is used.
        parser_factory (function): Factory function that returns a Parser
            object.  Its call signature must match that of the Parser
            constructor. If not present, ``Parser`` is used.
        builder_factory (function): Factory function that returns a Builder
            object.  Its call signature must match that of the Builder
            constructor. If not present, ``Builder`` is used.
        renderer_factory (function): Factory function that returns a Renderer
            object. Its call signature must match that of the Renderer
            constructor.  If not present, ``Renderer`` is used.
    '''

    def __init__(self, options=None, evaluator_factory=Evaluator,
                 parser_factory=Parser, builder_factory=Builder,
                 renderer_factory=Renderer):
        syspath = self._get_syspath_without_scriptdir()
        self._adjust_syspath(syspath)
        if options is None:
            options = FyppOptions()
        if inspect.signature(evaluator_factory) == inspect.signature(Evaluator):
            evaluator = evaluator_factory()
        else:
            raise FyppFatalError('evaluator_factory has incorrect signature')
        self._encoding = options.encoding
        if options.modules:
            self._import_modules(options.modules, evaluator, syspath,
                                 options.moduledirs)
        if options.defines:
            self._apply_definitions(options.defines, evaluator)
        if inspect.signature(parser_factory) == inspect.signature(Parser):
            parser = parser_factory(includedirs=options.includes,
                                    encoding=self._encoding)
        else:
            raise FyppFatalError('parser_factory has incorrect signature')
        if inspect.signature(builder_factory) == inspect.signature(Builder):
            builder = builder_factory()
        else:
            raise FyppFatalError('builder_factory has incorrect signature')

        fixed_format = options.fixed_format
        linefolding = not options.no_folding
        if linefolding:
            folding = 'brute' if fixed_format else options.folding_mode
            linelength = 72 if fixed_format else options.line_length
            indentation = 5 if fixed_format else options.indentation
            prefix = '&'
            suffix = '' if fixed_format else '&'
            linefolder = FortranLineFolder(linelength, indentation, folding,
                                           prefix, suffix)
        else:
            linefolder = DummyLineFolder()
        linenums = options.line_numbering
        contlinenums = (options.line_numbering_mode != 'nocontlines')
        self._create_parent_folder = options.create_parent_folder
        if inspect.signature(renderer_factory) == inspect.signature(Renderer):
            renderer = renderer_factory(
                evaluator, linenums=linenums, contlinenums=contlinenums,
                linenumformat=options.line_marker_format, linefolder=linefolder,
                filevarroot=options.file_var_root)
        else:
            raise FyppFatalError('renderer_factory has incorrect signature')
        self._preprocessor = Processor(parser, builder, renderer)


    def process_file(self, infile, outfile=None):
        '''Processes input file and writes result to output file.

        Args:
            infile (str): Name of the file to read and process. If its value is
                '-', input is read from stdin.
            outfile (str, optional): Name of the file to write the result to.
                If its value is '-', result is written to stdout. If not
                present, result will be returned as string.
            env (dict, optional): Additional definitions for the evaluator.

        Returns:
            str: Result of processed input, if no outfile was specified.
        '''
        infile = STDIN if infile == '-' else infile
        output = self._preprocessor.process_file(infile)
        if outfile is None:
            return output
        if outfile == '-':
            outfile = sys.stdout
        else:
            outfile = _open_output_file(outfile, self._encoding,
                                        self._create_parent_folder)
        outfile.write(output)
        if outfile != sys.stdout:
            outfile.close()
        return None


    def process_text(self, txt):
        '''Processes a string.

        Args:
            txt (str): String to process.
            env (dict, optional): Additional definitions for the evaluator.

        Returns:
            str: Processed content.
        '''
        return self._preprocessor.process_text(txt)


    @staticmethod
    def _apply_definitions(defines, evaluator):
        for define in defines:
            words = define.split('=', 1)
            name = words[0]
            value = None
            if len(words) > 1:
                try:
                    value = evaluator.evaluate(words[1])
                except Exception as exc:
                    msg = "exception at evaluating '{0}' in definition for " \
                          "'{1}'".format(words[1], name)
                    raise FyppFatalError(msg) from exc
            evaluator.define(name, value)


    def _import_modules(self, modules, evaluator, syspath, moduledirs):
        lookuppath = []
        if moduledirs is not None:
            lookuppath += [os.path.abspath(moddir) for moddir in moduledirs]
        lookuppath.append(os.path.abspath('.'))
        lookuppath += syspath
        self._adjust_syspath(lookuppath)
        for module in modules:
            evaluator.import_module(module)
        self._adjust_syspath(syspath)


    @staticmethod
    def _get_syspath_without_scriptdir():
        '''Remove the folder of the fypp binary from the search path'''
        syspath = list(sys.path)
        scriptdir = os.path.abspath(os.path.dirname(sys.argv[0]))
        if os.path.abspath(syspath[0]) == scriptdir:
            del syspath[0]
        return syspath


    @staticmethod
    def _adjust_syspath(syspath):
        sys.path = syspath


class FyppOptions(optparse.Values):

    '''Container for Fypp options with default values.

    Attributes:
        defines (list of str): List of variable definitions in the form of
            'VARNAME=VALUE'. Default: []
        includes (list of str): List of paths to search when looking for include
            files. Default: []
        line_numbering (bool): Whether line numbering directives should appear
            in the output. Default: False
        line_numbering_mode (str): Line numbering mode 'full' or 'nocontlines'.
            Default: 'full'.
        line_marker_format (str): Line marker format. Currently 'std',
            'cpp' and 'gfortran5' are supported, where 'std' emits ``#line``
            pragmas similar to standard tools, 'cpp' produces line directives as
            emitted by GNU cpp, and 'gfortran5' cpp line directives with a
            workaround for a bug introduced in GFortran 5. Default: 'cpp'.
        line_length (int): Length of output lines. Default: 132.
        folding_mode (str): Folding mode 'smart', 'simple' or 'brute'. Default:
            'smart'.
        no_folding (bool): Whether folding should be suppressed. Default: False.
        indentation (int): Indentation in continuation lines. Default: 4.
        modules (list of str): Modules to import at initialization. Default: [].
        moduledirs (list of str): Module lookup directories for importing user
            specified modules. The specified paths are looked up *before* the
            standard module locations in sys.path.
        fixed_format (bool): Whether input file is in fixed format.
            Default: False.
        encoding (str): Character encoding for reading/writing files. Allowed
            values are Pythons codec identifiers, e.g. 'ascii', 'utf-8', etc.
            Default: 'utf-8'. Reading from stdin and writing to stdout is always
            encoded according to the current locale and is not affected by this
            setting.
        create_parent_folder (bool): Whether the parent folder for the output
            file should be created if it does not exist. Default: False.
    '''

    def __init__(self):
        optparse.Values.__init__(self)
        self.defines = []
        self.includes = []
        self.line_numbering = False
        self.line_numbering_mode = 'full'
        self.line_marker_format = 'cpp'
        self.line_length = 132
        self.folding_mode = 'smart'
        self.no_folding = False
        self.indentation = 4
        self.modules = []
        self.moduledirs = []
        self.fixed_format = False
        self.encoding = 'utf-8'
        self.create_parent_folder = False
        self.file_var_root = None


class FortranLineFolder:

    '''Implements line folding with Fortran continuation lines.

    Args:
        maxlen (int, optional): Maximal line length (default: 132).
        indent (int, optional): Indentation for continuation lines (default: 4).
        method (str, optional): Folding method with following options:

            * ``brute``: folding with maximal length of continuation lines,
            * ``simple``: indents with respect of indentation of first line,
            * ``smart``: like ``simple``, but tries to fold at whitespaces.

        prefix (str, optional): String to use at the beginning of a continuation
            line (default: '&').
        suffix (str, optional): String to use at the end of the line preceding
            a continuation line (default: '&')
    '''

    def __init__(self, maxlen=132, indent=4, method='smart', prefix='&',
                 suffix='&'):
        # Line length should be long enough that contintuation lines can host at
        # east one character apart of indentation and two continuation signs
        minmaxlen = indent + len(prefix) + len(suffix) + 1
        if maxlen < minmaxlen:
            msg = 'Maximal line length less than {0} when using an indentation'\
                  ' of {1}'.format(minmaxlen, indent)
            raise FyppFatalError(msg)
        self._maxlen = maxlen
        self._indent = indent
        self._prefix = ' ' * self._indent + prefix
        self._suffix = suffix
        if method not in ['brute', 'smart', 'simple']:
            raise FyppFatalError('invalid folding type')
        if method == 'brute':
            self._inherit_indent = False
            self._fold_position_finder = self._get_maximal_fold_pos
        elif method == 'simple':
            self._inherit_indent = True
            self._fold_position_finder = self._get_maximal_fold_pos
        elif method == 'smart':
            self._inherit_indent = True
            self._fold_position_finder = self._get_smart_fold_pos


    def __call__(self, line):
        '''Folds a line.

        Can be directly called to return the list of folded lines::

            linefolder = FortranLineFolder(maxlen=10)
            linefolder('  print *, "some Fortran line"')

        Args:
            line (str): Line to fold.

        Returns:
            list of str: Components of folded line. They should be
                assembled via ``\\n.join()`` to obtain the string
                representation.
        '''
        if self._maxlen < 0 or len(line) <= self._maxlen:
            return [line]
        if self._inherit_indent:
            indent = len(line) - len(line.lstrip())
            prefix = ' ' * indent + self._prefix
        else:
            indent = 0
            prefix = self._prefix
        suffix = self._suffix
        return self._split_line(line, self._maxlen, prefix, suffix,
                                self._fold_position_finder)


    @staticmethod
    def _split_line(line, maxlen, prefix, suffix, fold_position_finder):
        # length of continuation lines with 1 or two continuation chars.
        maxlen1 = maxlen - len(prefix)
        maxlen2 = maxlen1 - len(suffix)
        start = 0
        end = fold_position_finder(line, start, maxlen - len(suffix))
        result = [line[start:end] + suffix]
        while end < len(line) - maxlen1:
            start = end
            end = fold_position_finder(line, start, start + maxlen2)
            result.append(prefix + line[start:end] + suffix)
        result.append(prefix + line[end:])
        return result


    @staticmethod
    def _get_maximal_fold_pos(_, __, end):
        return end


    @staticmethod
    def _get_smart_fold_pos(line, start, end):
        linelen = end - start
        ispace = line.rfind(' ', start, end)
        # The space we waste for smart folding should be max. 1/3rd of the line
        if ispace != -1 and ispace >= start + (2 * linelen) // 3:
            return ispace
        return end


class DummyLineFolder:

    '''Implements a dummy line folder returning the line unaltered.'''

    def __call__(self, line):
        '''Returns the entire line without any folding.

        Returns:
            list of str: Components of folded line. They should be
                assembled via ``\\n.join()`` to obtain the string
                representation.
        '''
        return [line]


def get_option_parser():
    '''Returns an option parser for the Fypp command line tool.

    Returns:
        OptionParser: Parser which can create an optparse.Values object with
            Fypp settings based on command line arguments.
    '''
    defs = FyppOptions()
    fypp_name = 'fypp'
    fypp_desc = 'Preprocesses source code with Fypp directives. The input is '\
                'read from INFILE (default: \'-\', stdin) and written to '\
                'OUTFILE (default: \'-\', stdout).'
    fypp_version = fypp_name + ' ' + VERSION
    usage = '%prog [options] [INFILE] [OUTFILE]'
    parser = optparse.OptionParser(prog=fypp_name, description=fypp_desc,
                                   version=fypp_version, usage=usage)

    msg = 'define variable, value is interpreted as ' \
          'Python expression (e.g \'-DDEBUG=1\' sets DEBUG to the ' \
          'integer 1) or set to None if omitted'
    parser.add_option('-D', '--define', action='append', dest='defines',
                      metavar='VAR[=VALUE]', default=defs.defines, help=msg)

    msg = 'add directory to the search paths for include files'
    parser.add_option('-I', '--include', action='append', dest='includes',
                      metavar='INCDIR', default=defs.includes, help=msg)

    msg = 'import a python module at startup (import only trustworthy modules '\
          'as they have access to an **unrestricted** Python environment!)'
    parser.add_option('-m', '--module', action='append', dest='modules',
                      metavar='MOD', default=defs.modules, help=msg)

    msg = 'directory to be searched for user imported modules before '\
          'looking up standard locations in sys.path'
    parser.add_option('-M', '--module-dir', action='append',
                      dest='moduledirs', metavar='MODDIR',
                      default=defs.moduledirs, help=msg)

    msg = 'emit line numbering markers'
    parser.add_option('-n', '--line-numbering', action='store_true',
                      dest='line_numbering', default=defs.line_numbering,
                      help=msg)

    msg = 'line numbering mode, \'full\' (default): line numbering '\
          'markers generated whenever source and output lines are out '\
          'of sync, \'nocontlines\': line numbering markers omitted '\
          'for continuation lines'
    parser.add_option('-N', '--line-numbering-mode', metavar='MODE',
                      choices=['full', 'nocontlines'],
                      default=defs.line_numbering_mode,
                      dest='line_numbering_mode', help=msg)

    msg = 'line numbering marker format,  currently \'std\', \'cpp\' and '\
          '\'gfortran5\' are supported, where \'std\' emits #line pragmas '\
          'similar to standard tools, \'cpp\' produces line directives as '\
          'emitted by GNU cpp, and \'gfortran5\' cpp line directives with a '\
          'workaround for a bug introduced in GFortran 5. Default: \'cpp\'.'
    parser.add_option('--line-marker-format', metavar='FMT',
                      choices=['cpp', 'gfortran5', 'std'],
                      dest='line_marker_format',
                      default=defs.line_marker_format, help=msg)

    msg = 'maximal line length (default: 132), lines modified by the '\
          'preprocessor are folded if becoming longer'
    parser.add_option('-l', '--line-length', type=int, metavar='LEN',
                      dest='line_length', default=defs.line_length, help=msg)

    msg = 'line folding mode, \'smart\' (default): indentation context '\
          'and whitespace aware, \'simple\': indentation context aware, '\
          '\'brute\': mechnical folding'
    parser.add_option('-f', '--folding-mode', metavar='MODE',
                      choices=['smart', 'simple', 'brute'], dest='folding_mode',
                      default=defs.folding_mode, help=msg)

    msg = 'suppress line folding'
    parser.add_option('-F', '--no-folding', action='store_true',
                      dest='no_folding', default=defs.no_folding, help=msg)

    msg = 'indentation to use for continuation lines (default 4)'
    parser.add_option('--indentation', type=int, metavar='IND',
                      dest='indentation', default=defs.indentation, help=msg)

    msg = 'produce fixed format output (any settings for options '\
          '--line-length, --folding-method and --indentation are ignored)'
    parser.add_option('--fixed-format', action='store_true',
                      dest='fixed_format', default=defs.fixed_format, help=msg)

    msg = 'character encoding for reading/writing files. Default: \'utf-8\'. '\
          'Note: reading from stdin and writing to stdout is encoded '\
          'according to the current locale and is not affected by this setting.'
    parser.add_option('--encoding', metavar='ENC', default=defs.encoding,
                      help=msg)

    msg = 'create parent folders of the output file if they do not exist'
    parser.add_option('-p', '--create-parents', action='store_true',
                      dest='create_parent_folder',
                      default=defs.create_parent_folder, help=msg)

    msg = 'in variables _FILE_ and _THIS_FILE_, use relative paths with DIR '\
          'as root directory. Note: the input file and all included files '\
          'must be in DIR or in a directory below.'
    parser.add_option('--file-var-root', metavar='DIR', dest='file_var_root',
                      default=defs.file_var_root, help=msg)

    return parser


def run_fypp():
    '''Run the Fypp command line tool.'''
    options = FyppOptions()
    optparser = get_option_parser()
    opts, leftover = optparser.parse_args(values=options)
    infile = leftover[0] if len(leftover) > 0 else '-'
    outfile = leftover[1] if len(leftover) > 1 else '-'
    try:
        tool = Fypp(opts)
        tool.process_file(infile, outfile)
    except FyppStopRequest as exc:
        sys.stderr.write(_formatted_exception(exc))
        sys.exit(USER_ERROR_EXIT_CODE)
    except FyppFatalError as exc:
        sys.stderr.write(_formatted_exception(exc))
        sys.exit(ERROR_EXIT_CODE)


def linenumdir_cpp(linenr, fname, flag=None):
    """Returns a GNU cpp style line directive.

    Args:
        linenr (int): Line nr (starting with zero).
        fname (str): File name.
        flag (str): Optional flag to print after the directive

    Returns:
        Line number directive as string.
    """
    if flag is None:
        return '# {0} "{1}"\n'.format(linenr + 1, fname)
    return '# {0} "{1}" {2}\n'.format(linenr + 1, fname, flag)


def linenumdir_std(linenr, fname, flag=None):
    """Returns standard #line pragma styled line directive.

    Args:
        linenr (int): Line nr (starting with zero).
        fname (str): File name.
        flag (str): Optional flag to print after the directive. Note, this
            option is only there to be API compatible with linenumdir_cpp(),
            but is ignored otherwise, since #line pragmas do not allow for
            extra file opening/closing flags.

    Returns:
        Line number directive as string.
    """
    return "#line {0} \"{1}\"\n".format(linenr + 1, fname)


def _shiftinds(inds, shift):
    return [ind + shift for ind in inds]


def _open_input_file(inpfile, encoding=None):
    try:
        inpfp = io.open(inpfile, 'r', encoding=encoding)
    except IOError as exc:
        msg = "Failed to open file '{0}' for read".format(inpfile)
        raise FyppFatalError(msg) from exc
    return inpfp


def _open_output_file(outfile, encoding=None, create_parents=False):
    if create_parents:
        parentdir = os.path.abspath(os.path.dirname(outfile))
        if not os.path.exists(parentdir):
            try:
                os.makedirs(parentdir)
            except OSError as exc:
                if exc.errno != errno.EEXIST:
                    msg = "Folder '{0}' can not be created"\
                        .format(parentdir)
                    raise FyppFatalError(msg) from exc
    try:
        outfp = io.open(outfile, 'w', encoding=encoding)
    except IOError as exc:
        msg = "Failed to open file '{0}' for write".format(outfile)
        raise FyppFatalError(msg) from exc
    return outfp


# Signature objects are available from Python 3.3 (and deprecated from 3.5)
def _get_callable_argspec(func):
    sig = inspect.signature(func)
    args = []
    defaults = {}
    varpos = None
    varkw = None
    for param in sig.parameters.values():
        if param.kind == param.POSITIONAL_OR_KEYWORD:
            args.append(param.name)
            if param.default != param.empty:
                defaults[param.name] = param.default
        elif param.kind == param.VAR_POSITIONAL:
            varpos = param.name
        elif param.kind == param.VAR_KEYWORD:
            varkw = param.name
        else:
            msg = "argument '{0}' has invalid argument type".format(param.name)
            raise FyppFatalError(msg)
    return args, defaults, varpos, varkw



def _blank_match(match):
    size = match.end() - match.start()
    return " " * size


def _argsplit_fortran(argtxt):
    txt = _INLINE_EVAL_REGION_REGEXP.sub(_blank_match, argtxt)
    splitpos = [-1]
    quote = None
    closing_brace_stack = []
    closing_brace = None
    for ind, char in enumerate(txt):
        if quote:
            if char == quote:
                quote = None
            continue
        if char in _QUOTES_FORTRAN:
            quote = char
            continue
        if char in _OPENING_BRACKETS_FORTRAN:
            closing_brace_stack.append(closing_brace)
            ind = _OPENING_BRACKETS_FORTRAN.index(char)
            closing_brace = _CLOSING_BRACKETS_FORTRAN[ind]
            continue
        if char in _CLOSING_BRACKETS_FORTRAN:
            if char == closing_brace:
                closing_brace = closing_brace_stack.pop(-1)
                continue
            else:
                msg = "unexpected closing delimiter '{0}' in expression '{1}' "\
                      "at position {2}".format(char, argtxt, ind + 1)
                raise FyppFatalError(msg)
        if not closing_brace and char == _ARGUMENT_SPLIT_CHAR_FORTRAN:
            splitpos.append(ind)
    if quote or closing_brace:
        msg = "open quotes or brackets in expression '{0}'".format(argtxt)
        raise FyppFatalError(msg)
    splitpos.append(len(txt))
    fragments = [argtxt[start + 1 : end]
                 for start, end in zip(splitpos, splitpos[1:])]
    return fragments


def _formatted_exception(exc):
    error_header_formstr = '{file}:{line}: '
    error_body_formstr = 'error: {errormsg} [{errorclass}]'
    if not isinstance(exc, FyppError):
        return error_body_formstr.format(
            errormsg=str(exc), errorclass=exc.__class__.__name__)
    out = []
    if exc.fname is not None:
        if exc.span[1] > exc.span[0] + 1:
            line = '{0}-{1}'.format(exc.span[0] + 1, exc.span[1])
        else:
            line = '{0}'.format(exc.span[0] + 1)
        out.append(error_header_formstr.format(file=exc.fname, line=line))
    out.append(error_body_formstr.format(errormsg=exc.msg,
                                         errorclass=exc.__class__.__name__))
    if exc.__cause__ is not None:
        out.append('\n' + _formatted_exception(exc.__cause__))
    out.append('\n')
    return ''.join(out)


if __name__ == '__main__':
    run_fypp()
