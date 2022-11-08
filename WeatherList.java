public class WeatherList {
    private static final int NUMDATA = 1440;
    private Weather weather[] = new Weather[NUMDATA];

    public void readWeather() {
        // assume temp between 20 and 40
        // assume precipitation between 0 and 20
        // assume wind between 0 and 20
        // assume humidity between 0 and 100
        float TempRange[] = { 20, 40 };
        float WindRange[] = { 0, 20 };
        float HumidityRange[] = { 0, 20 };
        float PrecipitationRange[] = { 0, 100 };

        for (int i = 0; i < weather.length; i++) {
            weather[i] = new Weather();
            System.out.println("Created a weather object: " + weather[i]);
            weather[i].generate(TempRange, WindRange, HumidityRange, PrecipitationRange);
            weather[i].display();
        }
    }

    // temperature
    public float minTemp() {
        float min = weather[0].getTemperature();
        for (int i = 1; i < NUMDATA; i++) {
            if (min > weather[i].getTemperature()) {
                min = weather[i].getTemperature();
            }
        }

        return min;
    }

    public float maxTemp() {
        float max = weather[0].getTemperature();
        for (int i = 1; i < NUMDATA; i++) {
            if (max < weather[i].getTemperature()) {
                max = weather[i].getTemperature();
            }
        }
        return max;
    }

    public float avgTemp() {
        float sum = 0;
        for (int i = 0; i < NUMDATA; i++) {
            sum += weather[i].getTemperature();
        }
        float avg = sum / NUMDATA;
        return avg;
    }

    public float stdDevTemp(float avg) {
        float sd = 0;
        for (int i = 0; i < NUMDATA; i++) {
            sd += Math.pow(weather[i].getTemperature() - avg, 2);
        }
        sd /= NUMDATA - 1;
        return (float) Math.sqrt(sd);
    }

    // precipitation
    public float minPrecipitation() {
        float min = weather[0].getPrecipitation();
        for (int i = 1; i < NUMDATA; i++) {
            if (min > weather[i].getPrecipitation()) {
                min = weather[i].getPrecipitation();
            }
        }
        return min;
    }

    public float maxPrecipitation() {
        float max = weather[0].getPrecipitation();
        for (int i = 1; i < NUMDATA; i++) {
            if (max < weather[i].getPrecipitation()) {
                max = weather[i].getPrecipitation();
            }
        }
        return max;
    }

    public float avgPrecipitation() {
        float sum = 0;
        for (int i = 0; i < NUMDATA; i++) {
            sum += weather[i].getPrecipitation();
        }
        float avg = sum / NUMDATA;
        return avg;
    }

    public float stdDevPrecipitation(float avg) {
        float sd = 0;
        for (int i = 0; i < NUMDATA; i++) {
            sd += Math.pow(weather[i].getPrecipitation() - avg, 2);
        }
        sd /= NUMDATA - 1;
        return (float) Math.sqrt(sd);
    }

    // humidity
    public float minHumidity() {
        float min = weather[0].getHumidity();
        for (int i = 1; i < NUMDATA; i++) {
            if (min > weather[i].getHumidity()) {
                min = weather[i].getHumidity();
            }
        }
        return min;
    }

    public float maxHumidity() {
        float max = weather[0].getHumidity();
        for (int i = 1; i < NUMDATA; i++) {
            if (max < weather[i].getHumidity()) {
                max = weather[i].getHumidity();
            }
        }
        return max;
    }

    public float avgHumidity() {
        float sum = 0;
        for (int i = 0; i < NUMDATA; i++) {
            sum += weather[i].getHumidity();
        }
        float avg = sum / NUMDATA;
        return avg;
    }

    public float stdDevHumidity(float avg) {
        float sd = 0;
        for (int i = 0; i < NUMDATA; i++) {
            sd += Math.pow(weather[i].getHumidity() - avg, 2);
        }
        sd /= NUMDATA - 1;
        return (float) Math.sqrt(sd);
    }

    // wind
    public float minWindSpeed() {
        float min = weather[0].getWind();
        for (int i = 1; i < NUMDATA; i++) {
            if (min > weather[i].getWind()) {
                min = weather[i].getWind();
            }
        }
        return min;
    }

    public float maxWindSpeed() {
        float max = weather[0].getTemperature();
        for (int i = 1; i < NUMDATA; i++) {
            if (max < weather[i].getWind()) {
                max = weather[i].getWind();
            }
        }
        return max;
    }

    public float avgWindSpeed() {
        float sum = 0;
        for (int i = 0; i < NUMDATA; i++) {
            sum += weather[i].getWind();
        }
        float avg = sum / NUMDATA;
        return avg;
    }

    public float stdDevWindSpeed(float avg) {
        float sd = 0;
        for (int i = 0; i < NUMDATA; i++) {
            sd += Math.pow(weather[i].getWind() - avg, 2);
        }
        sd /= NUMDATA - 1;
        return (float) Math.sqrt(sd);
    }

    // debug
    public void debugWeather() {
        for (int i = 0; i < NUMDATA; i++) {
            System.out.println("temperature data at index " + i + " is " + weather[i].getTemperature());
            System.out.println("precipitation data at index " + i + " is " + weather[i].getPrecipitation());
            System.out.println("humidity data at index " + i + " is " + weather[i].getHumidity());
            System.out.println("wind data at index " + i + " is " + weather[i].getWind());
        }
    }
}
