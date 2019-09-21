package ru.tinkoff.phobos

import com.fasterxml.aalto.{AsyncByteArrayFeeder, AsyncXMLStreamReader}

package object decoding {
  type XmlStreamReader = AsyncXMLStreamReader[AsyncByteArrayFeeder]
}
