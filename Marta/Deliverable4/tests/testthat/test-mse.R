test_that("mse works", {
  A<-c(1,1,1,1,1)
  B<-c(1,1,1,1,1)
  MSE<-mse(A,B)
  expect_equal(MSE,0)
})

test_that("mse works again",{
  A<-c(1,1,1,1,1)
  B<-c(-1,-1,-1,-1,-1)
  MSE<-mse(A,B)
  expect_false(MSE<0)
})
