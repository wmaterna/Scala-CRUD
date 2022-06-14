package models

case class CarModel(id: Long, name: String, yearOfProduction: Int, color: String, brandId: Long)
case class NewCarModel(name: String, yearOfProduction: Int, color: String, brandId: Long)

