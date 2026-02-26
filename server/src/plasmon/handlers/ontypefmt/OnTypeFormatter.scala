package plasmon.handlers.ontypefmt

import org.eclipse.lsp4j as l

// originally based on https://github.com/scalameta/metals/blob/2a6f8a437a1ce7c44140673edfe34bb74dfd33be/metals/src/main/scala/scala/meta/internal/metals/formatting/OnTypeFormattingProvider.scala#L27-L31
private abstract class OnTypeFormatter {
  def contribute(
    onTypeformatterParams: OnTypeFormatterParams
  ): Option[List[l.TextEdit]]
}
