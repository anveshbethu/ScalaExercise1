import scala.io.Source
import java.util.Date
import java.text.SimpleDateFormat
import scala.collection.mutable.HashMap

object HitsCount {
  
  type SiteHits = HashMap[String, Int]
  type DateSite = HashMap[String, SiteHits]
  
  def fetchDate(ds: => DateSite, date: String): SiteHits = {			// O(1) constant time as look up from HashMap
    if(ds.contains(date)) ds.apply(date)
    else {
      def siteHit: SiteHits = new SiteHits
      ds += (date -> siteHit)
      siteHit
    }
  }
  
  def addSite(hits: => SiteHits, site: String) = {				// O(1) constant time as look up from HashMap
    if(hits.contains(site)) hits(site) = hits.apply(site)+1
    else hits += (site -> 1)
  }
  
  def readData(line: String): (String, String) = {				// O(1) constant
    def epoToDate(epo: Long): String = {
      val simpleDate = new SimpleDateFormat("MM/dd/yyyy")
      simpleDate.format(new Date(epo*1000L))
    }
    val data = line.split('|')
    (epoToDate(data(0).toLong), data(1))
  }
  
  def printFormat(result: DateSite) = {						// O(n*m * log(n)*log(m)) 
      for((date, siteCount) <- result.toSeq.sortBy(_._1)) {			// O(n*log(n)) sorting n dates
        println(date + " GMT")
        for((site, count) <- siteCount.toSeq.sortWith(_._2 > _._2)) {		// O(m*mlog(m)) sorting m sites
          println(site + " "+ count)        
        } 
      }
    }
  
  def main(args: Array[String]){
    if (args.length != 1) {
    Console.err.println("Usage: sbt run <path to input file> ")
    System.exit(1)
    }
    val result: DateSite = new DateSite    
    for(line <- Source.fromFile(args(0)).getLines()){
      val (sdate, site): (String, String) = readData(line)
      addSite(fetchDate(result, sdate), site)
    }
    printFormat(result)
  }
}
