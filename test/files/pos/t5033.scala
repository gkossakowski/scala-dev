trait Fruit {
  type Seed
}

trait PipExtractor {
  def extract(f: Fruit): f.Seed
}

trait LaserGuidedPipExtractor extends PipExtractor {
  def extract(f: Fruit): f.Seed
}