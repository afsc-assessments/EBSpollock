.get_rmse <- function(A,dolog=TRUE)
{
  df <- list()
  if(dolog)
  {
  df$bio_bts <- sqrt(sum(log(A$ob_bts/A$eb_bts)^2)/length(A$ob_bts))
  df$tot_bts <- sqrt(sum(log(A$ot_bts/A$et_bts)^2)/length(A$ot_bts))
  df$bio_eit <- sqrt(sum(log(A$ob_eit/A$eb_eit)^2)/length(A$ob_eit))
  df$tot_eit <- sqrt(sum(log(A$ot_eit/A$et_eit)^2)/length(A$ot_eit))
  }
  else
  {
  df$bio_bts <- sqrt(sum((A$ob_bts-A$eb_bts)^2)/length(A$ob_bts))
  df$tot_bts <- sqrt(sum((A$ot_bts-A$et_bts)^2)/length(A$ot_bts))
  df$bio_eit <- sqrt(sum((A$ob_eit-A$eb_eit)^2)/length(A$ob_eit))
  df$tot_eit <- sqrt(sum((A$ot_eit-A$et_eit)^2)/length(A$ot_eit))

  }
  return(df)
}

 rbind(.get_rmse(mod15.1),.get_rmse(mod16.1))
