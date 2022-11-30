library(torch)

x = c(2.7, 4, 4.5, 4.6, 7.4)
f = function(x) sin(x) + sin((10 / 3) * x)
y = f(x)

x = torch_tensor(as.matrix(x), dtype = torch_float())
y = torch_tensor(as.matrix(y), dtype = torch_float())

input_dim = 1
n_input = 5

model = nn_sequential(
  nn_linear(input_dim, 50),
  nn_tanh(),

  nn_linear(50, 50),
  nn_tanh(),

  nn_linear(50, 50),
  nn_tanh(),

  nn_linear(50, 1)
)

criterion = nn_mse_loss()

optimizer = optim_adam(model$parameters, lr = 0.01)

epochs = 200

for (i in seq_len(epochs)) {
  optimizer$zero_grad()
  y_pred = model(x)
  loss = criterion(y_pred, y)
  loss$backward()
  optimizer$step()
}

phi = as.matrix(model[["5"]]$forward(model[["4"]]$forward(model[["3"]]$forward(model[["2"]]$forward(model[["1"]]$forward(model[["0"]]$forward(x)))))))

alpha = 1
beta = 1
y_ = as.matrix(y)  # FIXME: - nu(x) (9)

K = beta * t(phi) %*% phi + diag(alpha, nrow = 50L, ncol = 50L)
m = beta * solve(K) %*% t(phi) %*% y_

mu = t(m) %*% 
