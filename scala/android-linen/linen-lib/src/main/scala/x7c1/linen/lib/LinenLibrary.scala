package x7c1.linen.lib

import android.support.v7.app.ActionBarActivity
import android.view.View
import x7c1.linen.struct.LinenStruct

object LinenLibrary {

  def createMessageFor(name: String) = s"hello, $name!"

  def sample(x: ActionBarActivity): View = {
    x.getSupportActionBar.getCustomView
  }
}

class SampleImpl extends LinenStruct {
  override def getName: String = "hoge"
}
