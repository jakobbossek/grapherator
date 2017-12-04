context("graph generators")

test_that("graph generation: simple 2o graph", {
  # here we generate a complex biobjective graph problem
  # with both euclidean and random weights

  g = graph(lower = 0, upper = 100)
  g = addNodes(g, n = 50L, generator = addNodesUniform)
  g = addWeights(g, generator = addWeightsDistance, method = "euclidean", symmetric = TRUE)
  g = addWeights(g, generator = addWeightsRandom, method = runif, symmetric = TRUE)

  expect_class(g, "grapherator")
  expect_true(g$n.nodes == 50L)
  expect_true(g$n.clusters == 0L)
  expect_true(g$n.weights == 2L)
  expect_set_equal(g$weight.types, c("distance-euclidean", "random"))
  expect_true(isSymmetricMatrix(g$weights[[1L]]))
  expect_true(isSymmetricMatrix(g$weights[[2L]]))
  expect_output(print(g), regexp = "GRAPH")

  pls = plot(g)
  expect_list(pls, types = "ggplot", len = 2L, any.missing = FALSE, all.missing = FALSE)
})

test_that("graph generation: complex clustered graph", {
  g = graph(lower = 0, upper = 100)
  g = addNodes(g, n = 3L, generator = addNodesLHS)
  g = addNodes(g, n = 9L, by.centers = TRUE, generator = addNodesUniform, lower = c(0, 0), upper = c(1, 1))
  g = addNodes(g, n = 100L, generator = addNodesGrid)
  g = addWeights(g, generator = addWeightsRandom, method = rnorm, mean = 5, sd = 1.3)
  g = addWeights(g, generator = addWeightsDistance, method = "minkowski", p = 2.5, symmetric = FALSE)

  # check plotting of cluster centers
  pls = plot(g, show.cluster.centers = TRUE)
  expect_list(pls, types = "ggplot", len = 2L, any.missing = FALSE, all.missing = FALSE)

  g = addWeights(g, generator = addWeightsRandom, method = function(n) {
    sample(c(1, -10), n, replace = TRUE) * rexp(n, rate = 0.1) * 1:n
  })

  expect_class(g, "grapherator")
  expect_true(g$n.nodes == 130L)
  expect_true(g$n.clusters == 3L)
  expect_true(g$n.weights == 3L)
  expect_list(g$weights, types = "matrix", any.missing = FALSE, all.missing = FALSE, len = g$n.weights)
  expect_true(isSymmetricMatrix(g$weights[[1L]]))
  expect_true(isSymmetricMatrix(g$weights[[2L]]))
  expect_true(isSymmetricMatrix(g$weights[[3L]])) # distance based are always symmetric

  expect_error(plot(g), regexpr = "not supported")
})

test_that("graph generation: manual passing of coordinates weights works", {
  g = graph(lower = 0, upper = 10)
  center.coordinates = matrix(c(1, 2, 2, 5, 8, 3), byrow = TRUE, ncol = 2L)
  g = addNodes(g, coordinates = center.coordinates)
  g = addNodes(g, n = 9L, by.centers = TRUE, generator = addNodesGrid, lower = c(0, 0), upper = c(2, 2))
  expect_equal(center.coordinates, g$center.coordinates)
  weights = diag(30)
  g = addWeights(g, weights = weights)
  g = addWeights(g, generator = addWeightsRandom, method = rnorm, mean = 5, sd = 1.3)
  weights[1, 4] = 4
  g = addWeights(g, weights = weights)

  expect_class(g, "grapherator")
  expect_true(g$n.nodes == 30L)
  expect_true(g$n.clusters == 3L)
  expect_true(g$n.weights == 3L)
  expect_list(g$weights, types = "matrix", any.missing = FALSE, all.missing = FALSE, len = g$n.weights)
  expect_true(isSymmetricMatrix(g$weights[[1L]]))
  expect_true(isSymmetricMatrix(g$weights[[2L]]))
  expect_true(isSymmetricMatrix(g$weights[[3L]])) # converted to symmetric matrix internally
})


test_that("graph generation: check correct error messages", {
  expect_error(graph(lower = 10, upper = 5))

  g = graph(lower = 0, upper = 100)
  expect_error(addWeights(g, generator = addWeightsDistance, method = "euclidean"), regexp = "first place")
})