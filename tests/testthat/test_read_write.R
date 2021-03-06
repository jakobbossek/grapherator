context("{write,read}GP")

test_that("writeGP and readGP work well", {
  # test instance
  g = graph(lower = c(0, 0), upper = c(100, 100))
  g = addNodes(g, n = 3, generator = addNodesUniform)
  g = addNodes(g, n = 9, by.centers = TRUE, generator = addNodesUniform, lower = c(0, 0), upper = c(10, 10))
  g = addEdges(g, generator = addEdgesDelauney, type = "intracluster")
  g = addEdges(g, generator = addEdgesWaxman, type = "intercenter", alpha = 0.8, beta = 0.6)
  g = addWeights(g, generator = addWeightsCorrelated, rho = -0.9)


  filename = tempfile(fileext = ".graph")
  writeGP(g, filename)
  g2 = readGP(filename)
  unlink(filename)

  expect_equal(g$n.nodes, g2$n.nodes)
  expect_equal(g$n.clusters, g2$n.clusters)
  expect_equal(g$n.weights, g2$n.weights)
  expect_equal(g$membership, g2$membership)
  expect_equal(g$node.types, g2$node.types)
  expect_equal(g$edge.types, g2$edge.types)
  expect_equal(g$weight.types, g2$weight.types)
  expect_equal(length(g$weights), length(g2$weights))
  expect_true(all(dim(g$coordinates) == dim(g2$coordinates)))
  expect_true(all(g$adj.mat == g2$adj.mat))
})
