import scala.collection.mutable.ListBuffer

object Harvest extends App{

	def getInfo(arr:Array[Array[Integer]]) = {
		var max = arr(0)(0);
		var min = arr(0)(0);
		var max_date_set = new ListBuffer[(Int, Int)]();
		var min_date_set = new ListBuffer[(Int, Int)]();
		var pair_val = (0, 0);
		var total_harvest = 0;
		var average = 0F;
		var no_of_days = 0;
		var harvest_arr_one_dim = new ListBuffer[Integer]()

		for(i<-0 to 3; j<-0 to 6){
			if(arr(i)(j) == max){
				pair_val = (i, j);
				max_date_set += pair_val; 
			}else if(arr(i)(j) > max){
				max=arr(i)(j);
				max_date_set.clear();
				pair_val = (i, j);
				max_date_set += pair_val; 
			}

			if(arr(i)(j) == min){
				pair_val = (i, j);
				min_date_set += pair_val; 
			}else if(arr(i)(j) < min){
				min=arr(i)(j);
				min_date_set.clear();
				pair_val = (i, j);
				min_date_set += pair_val; 
			}

			total_harvest += arr(i)(j); 
			no_of_days += 1;
			harvest_arr_one_dim += arr(i)(j);
		}

		average = total_harvest.toFloat/no_of_days;
		var range = max - min;
		var sorted_arr = harvest_arr_one_dim.sortWith(_ < _);
		var median = (sorted_arr((28-1)/2) + sorted_arr(28/2)).toFloat/2

		(max, max_date_set, min, min_date_set, average, range, median, sorted_arr);
	}

	def getDay(dayInfo:scala.collection.mutable.ListBuffer[(Int, Int)])= {
		
		var dayName = "";
		var weekName = "";

		for(e <- dayInfo){
			var (week, day) = e;

			day match{
				case 0 => dayName = "Monday";
				case 1 => dayName = "Tuesday";
				case 2 => dayName = "Wednesday";
				case 3 => dayName = "Thursday";
				case 4 => dayName = "Friday";
				case 5 => dayName = "Saturday";
				case 6 => dayName = "Sunday";
			}
			week match{
				case 0 => weekName = "1st";
				case 1 => weekName = "2nd";
				case 2 => weekName = "3rd";
				case 3 => weekName = "4th";
			}

			println("---Found in " + dayName + " of " + weekName + " week.");
		}	
	}

	val arr = Array.ofDim[Integer](4, 7);

	val h = scala.util.Random;

	for(i<-0 to 3; j<-0 to 6){
		arr(i)(j) = h.nextInt(100);
	}

	println();

	for(i<-0 to 3; j<-0 to 6){
		print(arr(i)(j) + "\t");
		if(j == 6){
			println();
		}
	}

	val (maxVal, maxDateSet, minVal, minDateSet, averageVal, rangeVal, medianVal, sorted_arr) = getInfo(arr);

	println("\nMax harvest : " + maxVal + "Kg");
	getDay(maxDateSet);
	println("\nMin harvest : " + minVal + "Kg");
	getDay(minDateSet);
	println("\nRange  : " + rangeVal);
	println("Average: " + averageVal + "Kg/day");
	println("Median : " + medianVal + "\n");	
}