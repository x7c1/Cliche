package so.protocol.x7c1.plum.app

import so.protocol.x7c1.plum.lib.PlumLibrary
import javax.servlet.http.{HttpServletResponse, HttpServletRequest, HttpServlet}

class Plum extends HttpServlet {

  @Override
  override def doGet(request: HttpServletRequest, response: HttpServletResponse): Unit = {
    val message = PlumLibrary createMessageFor "plum"

    response setContentType "text/plain"
    response.getWriter println message
  }
}
