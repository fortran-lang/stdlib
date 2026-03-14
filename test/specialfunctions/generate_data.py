import numpy as np
import torch
import os

# create a directory to store the reference data
data_dir = 'data'
if not os.path.exists(data_dir):
    os.makedirs(data_dir)

# define a range of inputs to test
x_np = np.linspace(-10.0, 10.0, num=1001, dtype=np.float64)
x_torch = torch.tensor(x_np, requires_grad=True)

# map the activation functions available in stdlib to their PyTorch equivalents
activations = {
    'sigmoid': torch.sigmoid,
    'tanh': torch.tanh,
    'relu': torch.relu,
    'elu': torch.nn.functional.elu,
    'selu': torch.nn.functional.selu,
    'softplus': torch.nn.functional.softplus,
    'softsign': torch.nn.functional.softsign,
}

print('Generating reference data...')

for name, func in activations.items():
    if x_torch.grad is not None:
        x_torch.grad.zero_()
    
    y = func(x_torch)
    y.sum().backward()
    derivative = x_torch.grad.numpy()

    np.save(f'{data_dir}/{name}_input.npy', x_np)
    np.save(f'{data_dir}/{name}_output.npy', y.detach().numpy())
    np.save(f'{data_dir}/{name}_derivative.npy', derivative)
    
    print(f'Generated data for: {name}')

print('All reference data generated successfully.')
