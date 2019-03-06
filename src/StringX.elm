module StringX exposing (isBlank)


isBlank =
    String.trim >> String.isEmpty
