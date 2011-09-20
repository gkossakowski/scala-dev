object Test {
  val stores = List() map {  x =>
    object store {
      def single: store.type = this
    }
    store single
  }
}