RMSD=function(mm,oo)
{
  Rres=((sum((mm-oo)*(mm-oo)))/28)*0.5
  return(Rres)
}