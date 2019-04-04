package epsa.util

import java.io.ByteArrayOutputStream
import java.net.URI
import java.nio.charset.StandardCharsets
import org.apache.http.client.config.RequestConfig
import org.apache.http.client.entity.UrlEncodedFormEntity
import org.apache.http.client.methods.{CloseableHttpResponse, HttpGet, HttpPost, HttpUriRequest}
import org.apache.http.impl.client.HttpClients
import org.apache.http.message.BasicNameValuePair
import org.apache.http.util.EntityUtils
import scala.collection.JavaConverters._

/** HTTP helpers. */
object Http {

  /** Builds HTTP GET request. */
  def buildHttpGet(uri: String): HttpGet = {
    new HttpGet(new URI(uri))
  }

  /** Builds HTTP POST request. */
  def buildHttpPost(uri: String, params: Seq[(String, String)]): HttpPost = {
    val request = new HttpPost(new URI(uri))
    val pairs = params.map {
      case (key, value) => new BasicNameValuePair(key, value)
    }.toList
    val entity = new UrlEncodedFormEntity(pairs.asJava, StandardCharsets.UTF_8)
    request.setEntity(entity)
    request
  }

  /** Executes request and gets text response. */
  def executeTextRequest(request: HttpUriRequest): String = {
    executeRequest(request) { response =>
      EntityUtils.toString(response.getEntity)
    }
  }

  /** Executes request and gets binary response. */
  def executeBinaryRequest(request: HttpUriRequest): Array[Byte] = {
    executeRequest(request) { response =>
      val baos = new ByteArrayOutputStream()
      response.getEntity.writeTo(baos)
      baos.close()
      baos.toByteArray
    }
  }

  /** Executes request and processes response. */
  private def executeRequest[A](request: HttpUriRequest)(p: CloseableHttpResponse => A): A = {
    val timeout = epsa.Main.settings.httpClientTimeout.get
    val config = RequestConfig.custom()
      .setSocketTimeout(timeout.toMillis.toInt)
      .setConnectTimeout(timeout.toMillis.toInt)
      .build()
    val client = HttpClients.custom()
      .setDefaultRequestConfig(config)
      .build()
    try {
      val response = client.execute(request)
      val r = p(response)
      response.close()
      val statusCode = response.getStatusLine.getStatusCode
      if ((statusCode < 200) || (statusCode > 299)) {
        throw new Exception(s"Request failed: ${response.getStatusLine}")
      }
      r
    } finally {
      client.close()
    }
  }

}
