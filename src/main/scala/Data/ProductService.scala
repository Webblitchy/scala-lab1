package Data

trait ProductService:
  type BrandName = String
  type ProductName = String

  def getPrice(product: ProductName, brand: BrandName): Double
  def getDefaultBrand(product: ProductName): BrandName

class ProductImpl extends ProductService:
  // DONE - Part 2 Step 2
  def getPrice(product: ProductName, brand: String): Double = 
    product match
      case "croissant" =>  2.0
      case "biere" => brand match
        case "boxer" | "farmer" => 1.0
        case "wittekop" => 2.0
        case "punkipa" | "jackhammer" => 3.0
        case "tenebreuse" => 4.0

      
    
  def getDefaultBrand(product: ProductName): BrandName = 
    product match
      case "croissant" => "maison"
      case "biere" => "boxer"
    
end ProductImpl
