import java.util.Random; // we want to make use of an existing class - Random

public class Weather { // a class consists of data + operation
  // data
  private float temperature;
  private float wind;
  private float humidity;
  private float precipitation;

  // operation : must be tightly coupled to the data

  // constructor (no return type, name same as class)
  public Weather() {
    System.out.println("Constructor is called");
  }

  public void generate(float TempRange[], float WindRange[], float HumidityRange[], float PrecipitationRange[]) {
    Random rand = new Random();
    temperature = rand.nextFloat() * (TempRange[1] - TempRange[0]) + TempRange[0];
    wind = rand.nextFloat() * (WindRange[1] - WindRange[0]) + WindRange[0];
    humidity = rand.nextFloat() * (HumidityRange[1] - HumidityRange[0]) + HumidityRange[0];
    precipitation = rand.nextFloat() * (PrecipitationRange[1] - PrecipitationRange[0]) + PrecipitationRange[0];
  }

  public void display() {
    System.out.println("temperature: " + temperature);
    System.out.println("wind: " + wind);
    System.out.println("humidity: " + humidity);
    System.out.println("precipitation: " + precipitation);
  }

  // setter * getter for private attributes
  public float getTemperature() {
    return temperature;
  }

  public void setTemperature(float newTemperature) {
    // Condition checking can be done
    if (newTemperature > 50) {
      return;
    }
    temperature = newTemperature;
  }

  public float getWind() {
    return wind;
  }

  public void setWind(float newWind) {
    wind = newWind;
  }

  public float getHumidity() {
    return humidity;
  }

  public void setHumidity(float newHumidity) {
    humidity = newHumidity;
  }

  public float getPrecipitation() {
    return precipitation;
  }

  public void setPrecipitation(float newPrecipitation) {
    precipitation = newPrecipitation;
  }

}
