package plasmon.internal;

public final class DisableScala2Pc implements java.util.function.BooleanSupplier {
  @Override public boolean getAsBoolean() {
    return Boolean.getBoolean("plasmon.native.disableScala2Pc");
  }
}
