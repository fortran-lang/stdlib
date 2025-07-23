#!/bin/bash

echo "📦 GitHub Fork Collaboration Setup Script"

# Prompt for User's GitHub username

read -p "Enter the GitHub username of the fork owner (e.g., alice): " USER

# Prompt for the branch name

read -p "Enter the PR branch name (e.g., feature-branch): " BRANCH

# Add remotes

echo "🔗 Adding remotes..."

git remote add $USER https://github.com/$USER/stdlib.git

# Fetch and checkout the PR branch

echo "📥 Fetching branch '$BRANCH'..."

git fetch $USER

git checkout -b $BRANCH $USER/$BRANCH

# Done

echo "✅ Repo set up. You're now on '$BRANCH'."

echo "You can now make changes and push directly to $USER/stdlib:$BRANCH"