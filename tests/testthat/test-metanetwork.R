nodes <- metanetwork:::nodes
links <- metanetwork:::links
test_that("metanetwork error", {
  expect_error(metanetwork(nodes[, -1], links, "Columns `network`"))
  expect_error(metanetwork(nodes, links[, -1], "Columns `from`"))
  expect_error(metanetwork(nodes, links, legend = TRUE, export = FALSE))
})

test_that("basic plot", {
  # expect_snapshot_file( metanetwork(nodes, links,
  # legend = FALSE, export = TRUE, res = 72),
  # 'metanetwork.png' ) expect_snapshot_file(
  # metanetwork(nodes, links, legend = TRUE, export =
  # TRUE, res = 72), 'metanetwork_legend.png' )
})

# unlink('metanetwork.png')
# unlink('metanetwork_legend.png')
