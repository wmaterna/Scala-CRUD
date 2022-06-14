package controllers
import javax.inject._
import models.{CarModel, NewCarModel, CarBrand}
import play.api.libs.json._
import play.api.mvc.{Action, AnyContent, BaseController, ControllerComponents}

import scala.collection.mutable


@Singleton
class CarModelController @Inject()(val controllerComponents: ControllerComponents)
  extends BaseController {
  private val carModelsList = new mutable.ListBuffer[CarModel]()
  val mazda = CarBrand(1, "Mazda")
  val mercedes = CarBrand(2, "Mercedes")
  carModelsList += CarModel(1, "Mazda", 2006, "black", mazda.id)
  carModelsList += CarModel(2, "Mercedes", 2020, "red", mercedes.id)

  implicit val carModelJson = Json.format[CarModel]
  implicit val newCarModelJson = Json.format[NewCarModel]

  def getAll(): Action[AnyContent] = Action {
    if(carModelsList.isEmpty) {
      NoContent
    } else {
      Ok(Json.toJson(carModelsList))
    }
  }

  def getById(id: Long) = Action {
    val foundItem = carModelsList.find(_.id == id)
    foundItem match {
      case Some(item) => Ok(Json.toJson(item))
      case None => BadRequest
    }
  }




  def deleteElement(id: Long) = Action {
    val elementToDelete: Option[CarModel] = carModelsList.find(car => car.id == id)
    elementToDelete match {
      case Some(element) =>
        carModelsList.remove(carModelsList.indexOf(element))
        Accepted
      case None =>
        BadRequest
    }

  }

  def addNewItem() = Action { implicit request =>
    val content = request.body
    val jsonObject = content.asJson

    val carsList: Option[NewCarModel] = jsonObject.flatMap(Json.fromJson[NewCarModel](_).asOpt)

    carsList match {
      case Some(newItem) =>
        val nextId = carModelsList.map(_.id).max + 1
        val toBeAdded = CarModel(nextId, newItem.name, newItem.yearOfProduction, newItem.color, newItem.brandId)
        carModelsList += toBeAdded
        Created(Json.toJson(toBeAdded))
      case None =>
        BadRequest
    }
  }

  def updateCar(id: Long) = Action { implicit request =>
    val requestName = request.body.asJson.get
    val foundItem = carModelsList.find(_.id == id)
    foundItem match {
      case Some(item) =>
        val newItem = item.copy(name=(requestName \ "name").as[String],
          yearOfProduction = (requestName \ "yearOfProduction").as[Int], color=(requestName \ "color").as[String], brandId=(requestName \ "brandId").as[Long] )
        carModelsList.dropWhileInPlace(_.id == id)
        carModelsList += newItem
        Accepted(Json.toJson(newItem))
      case None => BadRequest
    }
  }

}
