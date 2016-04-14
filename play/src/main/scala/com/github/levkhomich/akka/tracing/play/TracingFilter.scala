package com.github.levkhomich.akka.tracing.play

import com.github.levkhomich.akka.tracing.SpanMetadata
import com.github.levkhomich.akka.tracing.http.TracingHeaders
import play.api.libs.iteratee.Iteratee
import play.api.mvc._

import scala.collection.Map
import scala.concurrent.ExecutionContext
import scala.util.Random

class TracingFilter(implicit ec: ExecutionContext) extends EssentialFilter with PlayControllerTracing {
  def apply(nextFilter: EssentialAction) = new TracedAction(nextFilter) {
    override def apply(requestHeader: RequestHeader) = {
      super.apply(requestHeader)

      nextFilter(requestHeader).map { result =>

        trace.finish(requestHeader)

        result
      }
    }
  }

  protected class TracedAction(delegateAction: EssentialAction) extends EssentialAction with RequestTaggingHandler {
    override def apply(request: RequestHeader): Iteratee[Array[Byte], Result] = {
      if (requestTraced(request)) {
        sample(request)
        addHttpAnnotations(request)
      }
      delegateAction(request)
    }

    lazy val serviceName = play.libs.Akka.system.name

    lazy val excludedQueryParams = Set.empty[String]

    lazy val excludedHeaders = Set.empty[String]

    protected def sample(request: RequestHeader): Unit = {
      trace.sample(request, serviceName)
    }

    protected def addHttpAnnotations(request: RequestHeader): Unit = {
      // TODO: use batching
      trace.recordKeyValue(request, "request.path", request.path)
      trace.recordKeyValue(request, "request.method", request.method)
      trace.recordKeyValue(request, "request.secure", request.secure)
      trace.recordKeyValue(request, "request.proto", request.version)
      trace.recordKeyValue(request, "client.address", request.remoteAddress)
      // TODO: separate cookie records
      request.queryString.filter {
        case (key, values) =>
          !excludedQueryParams(key)
      }.foreach {
        case (key, values) =>
          values.foreach(trace.recordKeyValue(request, "request.query." + key, _))
      }
      request.headers.toMap.filter {
        case (key, values) =>
          !excludedHeaders(key)
      }.foreach {
        case (key, values) =>
          values.foreach(trace.recordKeyValue(request, "request.headers." + key, _))
      }
    }

    protected def requestTraced(request: RequestHeader): Boolean =
      !request.path.startsWith("/assets")

    protected def extractTracingTags(request: RequestHeader): Map[String, String] = {
      val spanId = TracingHeaders.SpanId -> SpanMetadata.idToString(Random.nextLong)
      if (request.headers.get(TracingHeaders.TraceId).isEmpty)
        Map(TracingHeaders.TraceId -> SpanMetadata.idToString(Random.nextLong)) + spanId
      else
        TracingHeaders.All.flatMap(header =>
          request.headers.get(header).map(header -> _)).toMap + spanId
    }

    override def tagRequest(request: RequestHeader): RequestHeader = {
      if (requestTraced(request))
        request.copy(tags = request.tags ++ extractTracingTags(request))
      else
        request
    }
  }

}

