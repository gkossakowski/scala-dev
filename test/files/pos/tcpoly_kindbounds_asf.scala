sealed trait GenericList[T, M[_ <: T]] {
 type Transformed[N[MMA <: T]] <: GenericList[T, N]
}

trait GenericCons[U, M[_ <: U], T <: GenericList[U, M]] extends GenericList[U, M] {
  val list: GenericList[U, M]
  type Transformed[N[MMB <: U]] = GenericCons[U, N, list.Transformed[N]]
}