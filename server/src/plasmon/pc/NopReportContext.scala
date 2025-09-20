package plasmon.pc

import java.util.Optional
import scala.meta.pc.reports.{ReportContext, Reporter}

object NopReportContext extends ReportContext {
  private val nopReporter: Reporter = (_, _) => Optional.empty()
  def unsanitized(): Reporter       = nopReporter
  def bloop(): Reporter             = nopReporter
  def incognito(): Reporter         = nopReporter
}
