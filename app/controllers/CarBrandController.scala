package controllers
import javax.inject._
import models.{CarBrand, CarModel, NewCarBrand}
import play.api.libs.json._
import play.api.mvc.{Action, AnyContent, BaseController, ControllerComponents}

import scala.collection.mutable


@Singleton
class CarBrandController @Inject()(val controllerComponents: ControllerComponents)
  extends BaseController {
  private val carList = new mutable.ListBuffer[CarBrand]()
  carList += CarBrand(1, "Mazda")
  carList += CarBrand(2, "Mercedes")

  implicit val carJson = Json.format[CarBrand]
  implicit val newCarJson = Json.format[NewCarBrand]

  def getAll(): Action[AnyContent] = Action {
   if(carList.isEmpty) {
     NoContent
   } else {
     Ok(Json.toJson(carList))
   }
  }

  def getById(id: Long) = Action {
    val foundItem = carList.find(_.id == id)
    foundItem match {
      case Some(item) => Ok(Json.toJson(item))
      case None => NotFound
    }
  }




  def deleteElement(id: Long) = Action {
    val elementToDelete: Option[CarBrand] = carList.find(car => car.id == id)
    elementToDelete match {
      case Some(element) =>
        carList.remove(carList.indexOf(element))
        Accepted
      case None =>
        BadRequest
    }

  }

  def addNewItem() = Action { implicit request =>
    val content = request.body
    val jsonObject = content.asJson

    val carsList: Option[NewCarBrand] = jsonObject.flatMap(Json.fromJson[NewCarBrand](_).asOpt)

    carsList match {
      case Some(newItem) =>
        val nextId = carList.map(_.id).max + 1
        val toBeAdded = CarBrand(nextId, newItem.name)
        carList += toBeAdded
        Created(Json.toJson(toBeAdded))
      case None =>
        BadRequest
    }
  }

  def updateCar(id: Long) = Action { implicit request =>
    val requestName = request.body.asJson.get
    val foundItem = carList.find(_.id == id)
    foundItem match {
      case Some(item) =>
        val newItem = item.copy(name=(requestName \ "name").as[String])
        carList.dropWhileInPlace(_.id == id)
        carList += newItem
        Accepted(Json.toJson(newItem))
      case None => NotFound
    }
  }
}
