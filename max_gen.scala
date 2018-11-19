import scala.io.Source
import Array._
import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks._
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
object max_gen  {

	var txnGraph = ofDim[Boolean](26,110)
	var totalAtoms = 0;
	var arrAtoms = new Array[Boolean](26);
	var allAtoms = ""; 
	var minSup = 3;
	var txnCount= 0;
	var maximal = new ListBuffer[String]() 

	class KMP(pattern: Array[Char], R: Int = 256) {
	    private val M = pattern.length
			private val dfa = Array.ofDim[Int](R, M)
			dfa(pattern(0))(0) = 1
			private var X = 0

			for (j <- 1 until M; c <- 0 until R) {
				dfa(c)(j) = dfa(c)(X)
						dfa(pattern(j))(j) = j + 1
						X = dfa(pattern(j))(X)
			}

	def search(text: Array[Char]): Boolean = {
	    val M = pattern.length
			val N = text.length

			@tailrec
			def loop(i: Int, j: Int): (Int, Int) =
			if(i < N && j < M) loop(i + 1, dfa(text(i))(j)) else (i, j)

			val tuple = loop(0, 0)
			if(tuple._2 == M) true else false
	    }
	}
	
	
	def init() : Unit ={
			for(i <- 0 to 25){
				for( j <- 0 to 109) 
					txnGraph(i)(j)=false;
						arrAtoms(i)=false;
			}
	}
	def reader(): Unit = {
			for(line <- Source.fromFile("src/traxn.txt").getLines()){
				var len = line.length();
				for ( i <- 0 to len-1){
					var item = line.charAt(i);
					arrAtoms(item-'a')=true;
					txnGraph(item-'a')(txnCount)=true;
				}
				txnCount = txnCount+1;
			}
			println("Total records found : "+txnCount);
			for(i <- 0 to 25)
				if(arrAtoms(i)==true){
					allAtoms  = allAtoms + (i+'a').toChar;
					totalAtoms = totalAtoms+1;
				}
	}

	def getList( item : Char) : List[Int] = {
			var ret = new ListBuffer[Int]()      //mutable list , tolist changes it to immutable
					if(item == ' '){
						for(i <- 1 to 109)
							ret +=i;
								return ret.toList;
					}
			for(i <- 0 to txnCount-1)
				if(txnGraph(item-'a')(i)==true){
					ret += (i+1);
				}
			var retList = ret.toList;
			return retList;
	}

	def intersectionList(list1 : List[Int], list2 : List[Int]) : List[Int] ={
			var ret = new ListBuffer[Int]();
			for (l1 <- list1)
				for(l2 <- list2)
					if(l1==l2)
						ret +=l1;
						return ret.toList;
	}


/*	def generate(nChar : Int , itemString : Array[Char], nItemset: Int, baseItemset : List[Int]) : Unit =  {
			var flag = false;
			
			var ct=0;
			for(i <- nChar to totalAtoms-1){
				var baseItemset2 = getList(allAtoms.charAt(i));
				var genItemset = intersectionList(baseItemset, baseItemset2);
				if(genItemset.size >= minSup){
					flag = true;
					itemString(nItemset) = allAtoms.charAt(i);
					generate(i+1,itemString,nItemset+1,genItemset);
				}  
			}

			var supersetFlag=true
					if(flag == false){
						//superset checking for str
						itemString(nItemset)='\0';
						if(maximal.size==0)
							supersetFlag=true
							val item = itemString.mkString("")

							supersetFlag=getSupersetFlag(maximal,item)
							if(supersetFlag){
								maximal.append(itemString.mkString(""))
								println(itemString.mkString(""))
								println(baseItemset.size) 
							}
					}
	}
*/
	def getSupersetFlag(maximal:ListBuffer[String],item:String) :Boolean={
			for (max<-maximal){
				var subsetFlag = item.forall { 
					x => max.contains(x)||x=='\0'
				}

				if (subsetFlag==true) {
					return false;
				}
			}
			return true;
	}

	def ayush(preflist:ArrayBuffer[String], preftid: ArrayBuffer[List[Int]], nItems:Int): Unit={
		var flag=false;
		var ct=0;
		var s="";
		var s1="";
		var s2="";
		for(i <- 0 to nItems-1){
			ct=0;
			var strlist = new ArrayBuffer[String]()
			var tidlist = new ArrayBuffer[List[Int]]()
			for(j <- i+1 to nItems-1){
				s1 = preflist(i);
				s2 = preflist(j);
				s = s1 +s2(s2.length()-1);
				var tid = intersectionList(preftid(i),preftid(j));
				if(tid.size >= minSup){
					strlist += s;
					tidlist += tid;
					ct = ct +1;
				}
			}
			//println("coutn is :"+ct);
			//println(strlist);
			//println(tidlist);
			if(ct>0)
				ayush(strlist,tidlist,ct);
			else{
				s = preflist(i);
				var supersetFlag = true;
				supersetFlag = getSupersetFlag(maximal,s);
				if(supersetFlag){
					maximal.append(s);
					println("maximal : "+ s);
				}
			}
		}
	}

	def main(arg: Array[String]) = {
		println("execution begin....")
		init();
		reader();
		var str = new Array[Char](totalAtoms+1);
		var nChar = 0; //iterator of items array
		var nItemset = 0; // iterator of the currently formed itemset
		var itemset = getList(' '); 
		//generate(nChar , str, nItemset, itemset);
		var strlist = new ArrayBuffer[String]()
		var tidlist = new ArrayBuffer[List[Int]]()
		for (i <- 0 to totalAtoms-1){
			strlist += allAtoms.charAt(i).toString;
			var tid = getList(allAtoms.charAt(i));
			tidlist += tid;
		}
		//println(strlist);
		//println(tidlist(0));
		ayush(strlist,tidlist, totalAtoms);
		//maximal.foreach { x => println(x) }

	}
}