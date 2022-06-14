package controllers
import javax.inject._
import models.{CarBrand, CarSalon, NewCarBrand, NewCarSalon}
import play.api.libs.json._
import play.api.mvc.{Action, AnyContent, BaseController, ControllerComponents}

import scala.collection.mutable


@Singleton
class CarSalonController @Inject()(val controllerComponents: ControllerComponents)
  extends BaseController {
  private val salonList = new mutable.ListBuffer[CarSalon]()
  salonList += CarSalon(1, "Salon BMW","Kraków, Krakowska 54")
  salonList += CarSalon(2, "salon Mercedes-Benz","Wrocław, Nowomiejska 65")

  implicit val salonJson = Json.format[CarSalon]
  implicit val newSalonJson = Json.format[NewCarSalon]

  def getAll(): Action[AnyContent] = Action {
    if(salonList.isEmpty) {
      NoContent
    } else {
      Ok(Json.toJson(salonList))
    }
  }

  def getById(id: Long) = Action {
    val foundItem = salonList.find(_.id == id)
    foundItem match {
      case Some(item) => Ok(Json.toJson(item))
      case None => NotFound
    }
  }




  def deleteElement(id: Long) = Action {
    val elementToDelete: Option[CarSalon] = salonList.find(car => car.id == id)
    elementToDelete match {
      case Some(element) =>
        salonList.remove(salonList.indexOf(element))
        Accepted
      case None =>
        BadRequest
    }

  }

  def addNewItem() = Action { implicit request =>
    val content = request.body
    val jsonObject = content.asJson

    val carsList: Option[NewCarSalon] = jsonObject.flatMap(Json.fromJson[NewCarSalon](_).asOpt)

    carsList match {
      case Some(newItem) =>
        val nextId = salonList.map(_.id).max + 1
        val toBeAdded = CarSalon(nextId, newItem.name, newItem.address)
        salonList += toBeAdded
        Created(Json.toJson(toBeAdded))
      case None =>
        BadRequest
    }
  }

  def updateSalon(id: Long) = Action { implicit request =>
    val requestName = request.body.asJson.get
    val foundItem = salonList.find(_.id == id)
    foundItem match {
      case Some(item) =>
        val newItem = item.copy(name=(requestName \ "name").as[String], address=(requestName \ "address").as[String])
        salonList.dropWhileInPlace(_.id == id)
        salonList += newItem
        Accepted(Json.toJson(newItem))
      case None => NotFound
    }
  }
}
