package pratice


import java.net.{URI, URL}

import scala.io.Source


object EnumLogType extends Enumeration
{
    // 用户行为日志类型
    type EnumLogType = Value
    // 首页点击日志
    val Ajax2 = Value("ajax2")
    // 首页搜索日志
    val Ajax56 = Value("ajax56")
    // 猜你喜欢广告点击日志
    val Ajax131 = Value("ajax131")
    // 浏览器日志
    val Browser = Value("Browser")
}

object UrlTools
{
    /**
      * 获取网址的域名
      *
      * @param url String 要处理的域名
      * @return String 域名地址，获取失败则返回""
      */
    def getHost(url: String): String =
    {
        val host = try
        {
            new URL(url).getHost
        }
        catch
        {
            case _: Throwable => try
            {
                new URI(url).getHost
            }
            catch
            {
                case _: Throwable => ""
            }
        }
        if (host == null) "" else host
    }

    /**
      * 对url做utf8解码
      *
      * @param url url地址
      * @return 解码后的地址，如果解析失败返回原地址
      */
    def urlDecode(url: String): String =
    {
        try
        {
            java.net.URLDecoder.decode(url, "utf-8")
        }
        catch
        {
            case e: Throwable => url
        }
    }
}

case class DfUserLog(uid: String, logTime: Long, lo: String, ip: String, logType: String)
{
    def getBehaviorLabel: String = lo // List("游戏", "生活", "财经", "购物", "旅游", "视频", "军事", "汽车", "")(util.Random.nextInt(9))
}

object Utils
{
    private val _pregIp = "^(?:(?:(?:25[0-5]|2[0-4]\\d|(?:(?:1\\d{2})|(?:[1-9]?\\d)))\\.){3}(?:25[0-5]|2[0-4]\\d|(?:(?:1\\d{2})|(?:[1-9]?\\d))))$".r

    def validIp(ip: String): Boolean = _pregIp.findFirstIn(ip).isDefined

    private val _pregTime = "^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}$".r

    def validDateTime(time: String): Boolean = _pregTime.findFirstIn(time).isDefined
}

object ParseLog2DF
{
    private def _ajaxLog3Parse(log: String): (String, String) =
    {
        val tmp = log.split("=", 2)
        if (tmp.length == 1) (tmp(0).toLowerCase, "")
        else (tmp(0).toLowerCase, tmp(1).replace("%26", "&"))
    }

    private val _timeFormat = new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
    private val _uidPreg = "^[A-F0-9]{28}$".r

    def ajax2Log: (String) => Option[DfUserLog] = ajaxCommon(_, EnumLogType.Ajax2)

    def ajax56Log: (String) => Option[DfUserLog] = ajaxCommon(_, EnumLogType.Ajax56)

    def ajaxCommon(log: String, logType: EnumLogType.EnumLogType): Option[DfUserLog] =
    {
        val tmp1 = log.split("\t", 3)
        if (tmp1.length < 3 || !Utils.validIp(tmp1(1)) || !Utils.validDateTime(tmp1(0)))
            null
        else
        {
            val tmp2 = tmp1(2).split("&").map(_ajaxLog3Parse).toMap
            val uid = tmp2.getOrElse("uid", "")
            if (_uidPreg.findFirstIn(uid).isEmpty) null
            else Some(DfUserLog(uid,
                _timeFormat.parse(tmp1(0)).getTime,
                tmp2.getOrElse("lo", ""),
                tmp1(1),
                logType.toString))
        }
    }

    private val _httpPreg = "^[a-zA-Z]+:".r

    def browserLog(log: String): Option[DfUserLog] =
    {
        val ary = log.split("\t")
        if (ary.length >= 4 && ary(3).length > 0 && _uidPreg.findFirstIn(ary(2)).isDefined && Utils.validDateTime(ary(0)) && Utils.validIp(
            ary(1)))
        {
            val lo = if (_httpPreg.findFirstIn(ary(3)).isEmpty) UrlTools.urlDecode(ary(3).toLowerCase) else ary(3).toLowerCase
            Some(DfUserLog(ary(2), _timeFormat.parse(ary(0)).getTime, lo, ary(1), EnumLogType.Browser.toString))
        }
        else
        {
            None
        }
    }
}


/**
  * 用户某个分类的RFM偏好信息
  *
  * @param cat       分类名称
  * @param actCount  行为数
  * @param firstTime 首次访问时间
  * @param lastTime  最近一次访问时间
  * @author 王锦
  */
sealed class CatFavor(val cat: String,
                      val actCount: Map[String, Long],
                      val firstTime: Long,
                      val lastTime: Long)
{
    def mergeCat(other: CatFavor): CatFavor = this match
    {
        case DefaultCatFavor(_) => other
        case _ => other match
        {
            case DefaultCatFavor(_) => this
            case _ => new CatFavor(this.cat,
                this.actCount.foldLeft(other.actCount)
                {
                    case (m, (k, v)) => m + (k -> (m.getOrElse(k, 0L) + v))
                },
                Math.min(this.firstTime, other.firstTime),
                Math.max(this.lastTime, other.lastTime))
        }
    }
}

/**
  * 用户某个分类的RFM初始偏好
  *
  * @param cat 分类名称
  * @author 王锦
  */
case class DefaultCatFavor(override val cat: String) extends CatFavor(cat, Map.empty, Long.MaxValue, 0)


/**
  * 用户的RFM偏好模型
  *
  * @param behavior  用户行为数
  * @param favorList 偏好映射表
  */
sealed class DfUserFavor(val behavior: Long, val favorList: Map[String, CatFavor])
{
    def addUserLog(dfUserLog: DfUserLog): DfUserFavor =
    // 如果汇总的用户行为数过多就舍弃了
        if (this.behavior < DfUserFavor.maxUserBehaviorCount)
        {
            val favor = dfUserLog.getBehaviorLabel
            if (favor.length == 0) new DfUserFavor(this.behavior + 1, this.favorList)
            else new DfUserFavor(this.behavior + 1,
                this.favorList
                    + (favor -> this.favorList.getOrElse(favor, DefaultCatFavor(favor): CatFavor)
                    .mergeCat(new CatFavor(favor, Map(dfUserLog.logType -> 1), dfUserLog.logTime, dfUserLog.logTime))
                    ))
        }
        else
            DfUserTooAct

    def mergeUserFavor(other: DfUserFavor): DfUserFavor = this match
    {
        case DfUserTooAct => this
        case _ => other match
        {
            case DfUserTooAct => other
            case _ if this.behavior + other.behavior < DfUserFavor.maxUserBehaviorCount => new DfUserFavor(this.behavior + other.behavior,
                this.favorList.foldLeft(other.favorList)
                {
                    case (m, (k, v)) => if (m.contains(k)) m + (k -> v.mergeCat(m(k))) else m + (k -> v)
                })
            case _ => DfUserTooAct
        }
    }
}

case object DfUserTooAct extends DfUserFavor(0, Map.empty)

case object DfDeaultUserFavor extends DfUserFavor(0, Map.empty)

object DfUserFavor
{
    def maxUserBehaviorCount = 1000
}

/**
  * Created by wangj on 2017/5/31.
  */
object TestObj
{
    def main(args: Array[String]): Unit =
    {
        val tmp = Source.fromFile("D:/tmplog/ajax56.log")
            .getLines()
            .filter(_.length > 0)
            .map(ParseLog2DF.ajax56Log)
            .filter(_.isDefined)
            .map(_.get)
            .aggregate(DfDeaultUserFavor: DfUserFavor)((f, l) => f.addUserLog(l), (f1, f2) => f1.mergeUserFavor(f2))
        println(tmp)
    }
}
