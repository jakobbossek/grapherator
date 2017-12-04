set.seed(1)

genRandomPrueferCode = function(g) {
  sample(1:g$n.nodes, size = g$n.nodes - 2L, replace = TRUE)
}

genRandomPermutation = function(n) {
  sample(1:n)
}

isSymmetricMatrix = function(mat) {
  all(mat == t(mat))
}

objfunMCMST = function(pcode, instance) {
  getWeight(instance, prueferToEdgeList(pcode))
}

objfunMCTSP = function(perm, instance) {
  w1 = w2 = 0
  # add start node which is the end node as well
  perm = c(perm, 1L)
  # now add up weights
  for (i in 1:instance$n.nodes) {
    w1 = w1 + instance$weights[[1L]][perm[i], perm[i + 1L]]
    w2 = w2 + instance$weights[[2L]][perm[i], perm[i + 1L]]
  }
  return(c(w1, w2))
}
