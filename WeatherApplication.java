public class WeatherApplication { // a class consists of data + operation

  public static void main(String args[]) {
    // class object/instance
    // Weather weather = new Weather();

    // weather.generate(TempRange, WindRange, HumidityRange, PrecipitationRange);
    // System.out.println("Created a weather object: " + weather);
    // weather.display();

    float TempRange[] = { 20, 40 };
    float WindRange[] = { 0, 20 };
    float HumidityRange[] = { 0, 20 };
    float PrecipitationRange[] = { 0, 100 };

    Weather readings[] = new Weather[60];
    for (int i = 0; i < readings.length; i++) {
		readings[i] = new Weather();
		System.out.println("Created a weather object: " + readings[i]);
		readings[i].generate(TempRange, WindRange, HumidityRange, PrecipitationRange);
		readings[i].setTemperature(70);
		System.out.println("temperature data at index " + i + " is " + readings[i].getTemperature());
		readings[i].display();
    }

    // WeatherList wl = new WeatherList();
    // wl.readWeather();
  }
}
