context("{write,read}GP")

test_that("writeGP and readGP work well", {
  # test instance
  g = mcGP(lower = c(0, 0), upper = c(100, 100))
  g = addCoordinates(g, n = 3, generator = coordUniform)
  g = addCoordinates(g, n = 9, by.centers = TRUE, generator = coordUniform, lower = c(0, 0), upper = c(10, 10))
  g = addEdges(g, method = addEdgesDelauney, type = "intracluster")
  g = addEdges(g, method = addEdgesWaxman, type = "intercluster", alpha = 0.8, beta = 0.6)
  g = addWeights(g, method = "correlated", rho = -0.9)

  filename = "test.mcgp"
  writeGP(g, filename)
  g2 = readGP(filename)

  expect_equal(g$n.nodes, g2$n.nodes)
  expect_equal(g$n.clusters, g2$n.clusters)
  expect_equal(g$n.weights, g2$n.weights)
  expect_equal(g$membership, g2$membership)
  expect_equal(length(g$weights), length(g2$weights))
  expect_true(all(dim(g$coordinates) == dim(g2$coordinates)))
  expect_true(all(g$adj.mat == g2$adj.mat))
  unlink(filename)
})
