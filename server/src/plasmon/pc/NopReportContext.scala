package plasmon.pc

import scala.meta.internal.metals.{EmptyReportContext, ReportContext}

/** A report context that discards reports (Metals' internal flavor, that Mtags and Docstrings need)
  */
val NopReportContext: ReportContext = EmptyReportContext
