<?php
/**
 * Coordinate Bounds Class
 *
 * PHP version 5.3
 *
 * @category  Location
 * @author    Marcus T. Jaschen <mjaschen@gmail.com>
 * @copyright 2013 r03.org
 * @license   http://www.opensource.org/licenses/mit-license MIT License
 * @link      http://r03.org/
 */

/**
 * Coordinate Bounds Class
 *
 * @category Location
 * @author   Marcus T. Jaschen <mjaschen@gmail.com>
 * @license  http://www.opensource.org/licenses/mit-license MIT License
 * @link     http://r03.org/
 */
class Bounds
{
    /**
     * @var Coordinate
     */
    protected $northWest;

    /**
     * @var Coordinate
     */
    protected $southEast;

    /**
     * @param Coordinate $northWest
     * @param Coordinate $southEast
     */
    public function __construct(Coordinate $northWest, Coordinate $southEast)
    {
        $this->northWest = $northWest;
        $this->southEast = $southEast;
    }

    /**
     * Getter
     *
     * @return Coordinate
     */
    public function getNorthWest()
    {
        return $this->northWest;
    }

    /**
     * Getter
     *
     * @return Coordinate
     */
    public function getSouthEast()
    {
        return $this->southEast;
    }

    /**
     * @return float
     */
    public function getNorth()
    {
        return $this->northWest->getLat();
    }

    /**
     * @return float
     */
    public function getSouth()
    {
        return $this->southEast->getLat();
    }

    /**
     * @return float
     */
    public function getWest()
    {
        return $this->northWest->getLng();
    }

    /**
     * @return float
     */
    public function getEast()
    {
        return $this->southEast->getLng();
    }

    /**
     * Calculates the center of this bounds object and returns it as a
     * Coordinate instance.
     *
     * @return Coordinate
     */
    public function getCenter()
    {
        $centerLat = ($this->getNorth() + $this->getSouth()) / 2;
        $centerLng = ($this->getEast() + $this->getWest()) / 2;

        $overlap = $this->getWest() > 0 && $this->getEast() < 0;

        if ($overlap && $centerLng > 0) {
            $centerLng = -180 + $centerLng;
        } elseif ($overlap && $centerLng < 0) {
            $centerLng = 180 + $centerLng;
        } elseif ($overlap && $centerLng == 0) {
            $centerLng = 180;
        }

        return new Coordinate($centerLat, $centerLng);
    }
}
/**
 * Coordinate Implementation
 *
 * PHP version 5.3
 *
 * @category  Location
 * @author    Marcus T. Jaschen <mjaschen@gmail.com>
 * @copyright 2013 r03.org
 * @license   http://www.opensource.org/licenses/mit-license MIT License
 * @link      http://r03.org/
 */

/**
 * Coordinate Implementation
 *
 * @category Location
 * @author   Marcus T. Jaschen <mjaschen@gmail.com>
 * @license  http://www.opensource.org/licenses/mit-license MIT License
 * @link     http://r03.org/
 */
class Coordinate
{
    /**
     * @var float
     */
    protected $lat;

    /**
     * @var float
     */
    protected $lng;

    /**
     * @var Ellipsoid
     */
    protected $ellipsoid;

    /**
     * @param float     $lat       -90.0 .. +90.0
     * @param float     $lng       -180.0 .. +180.0
     * @param Ellipsoid $ellipsoid if omitted, WGS-84 is used
     *
     * @throws \InvalidArgumentException
     */
    public function __construct($lat, $lng, Ellipsoid $ellipsoid = null)
    {
        if (! $this->isValidLatitude($lat)) {
            throw new InvalidArgumentException("Latitude value must be numeric -90.0 .. +90.0");
        }

        if (! $this->isValidLongitude($lng)) {
            throw new InvalidArgumentException("Latitude value must be numeric -180.0 .. +180.0");
        }

        $this->lat = doubleval($lat);
        $this->lng = doubleval($lng);

        if ($ellipsoid !== null) {
            $this->ellipsoid = $ellipsoid;
        } else {
            $this->ellipsoid = Ellipsoid::createDefault();
        }
    }

    /**
     * @return float
     */
    public function getLat()
    {
        return $this->lat;
    }

    /**
     * @return float
     */
    public function getLng()
    {
        return $this->lng;
    }

    /**
     * @return Ellipsoid
     */
    public function getEllipsoid()
    {
        return $this->ellipsoid;
    }

    /**
     * Calculates the distance between the given coordinate
     * and this coordinate.
     *
     * @param Coordinate        $coordinate
     * @param DistanceInterface $calculator instance of distance calculation class
     *
     * @return float
     */
    public function getDistance(Coordinate $coordinate, DistanceInterface $calculator)
    {
        return $calculator->getDistance($this, $coordinate);
    }

    /**
     * @param FormatterInterface $formatter
     *
     * @return mixed
     */
    public function format(FormatterInterface $formatter)
    {
        return $formatter->format($this);
    }

    /**
     * Validates latitude
     *
     * @param mixed $latitude
     *
     * @return bool
     */
    protected function isValidLatitude($latitude)
    {
        if (! is_numeric($latitude)) {
            return false;
        }

        if ($latitude < - 90.0 || $latitude > 90.0) {
            return false;
        }

        return true;
    }

    /**
     * Validates longitude
     *
     * @param mixed $longitude
     *
     * @return bool
     */
    protected function isValidLongitude($longitude)
    {
        if (! is_numeric($longitude)) {
            return false;
        }

        if ($longitude < - 180.0 || $longitude > 180.0) {
            return false;
        }

        return true;
    }
}

/**
 * Interface for Distance Calculator Classes
 *
 * PHP version 5.3
 *
 * @category  Location
 * @package   Distance
 * @author    Marcus T. Jaschen <mjaschen@gmail.com>
 * @copyright 2013 r03.org
 * @license   http://www.opensource.org/licenses/mit-license MIT License
 * @link      http://r03.org/
 */


/**
 * Interface for Distance Calculator Classes
 *
 * @category Location
 * @package  Distance
 * @author   Marcus T. Jaschen <mjaschen@gmail.com>
 * @license  http://www.opensource.org/licenses/mit-license MIT License
 * @link     http://r03.org/
 */
interface DistanceInterface
{
    /**
     * @param Coordinate $point1
     * @param Coordinate $point2
     *
     * @return float distance between the two coordinates in meters
     */
    public function getDistance(Coordinate $point1, Coordinate $point2);
}

/**
 * Implementation of distance calculation with http://en.wikipedia.org/wiki/Law_of_haversines
 *
 * PHP version 5.3
 *
 * @category  Location
 * @package   Distance
 * @author    Marcus T. Jaschen <mjaschen@gmail.com>
 * @copyright 2013 r03.org
 * @license   http://www.opensource.org/licenses/mit-license MIT License
 * @link      http://r03.org/
 */


/**
 * Implementation of distance calculation with http://en.wikipedia.org/wiki/Law_of_haversines
 *
 * @see http://en.wikipedia.org/wiki/Law_of_haversines
 *
 * @category Location
 * @package  Distance
 * @author   Marcus T. Jaschen <mjaschen@gmail.com>
 * @license  http://www.opensource.org/licenses/mit-license MIT License
 * @link     http://r03.org/
 */
class Haversine implements DistanceInterface
{
    /**
     * @param Coordinate $point1
     * @param Coordinate $point2
     *
     * @throws NotMatchingEllipsoidException
     *
     * @return float
     */
    public function getDistance(Coordinate $point1, Coordinate $point2)
    {
        if ($point1->getEllipsoid() != $point2->getEllipsoid()) {
            throw new NotMatchingEllipsoidException("The ellipsoids for both coordinates must match");
        }

        $lat1 = deg2rad($point1->getLat());
        $lat2 = deg2rad($point2->getLat());
        $lng1 = deg2rad($point1->getLng());
        $lng2 = deg2rad($point2->getLng());

        $dLat = $lat2 - $lat1;
        $dLng = $lng2 - $lng1;

        $radius = $point1->getEllipsoid()->getArithmeticMeanRadius();

        $s = 2 * $radius * asin(
            sqrt(
                pow(sin($dLat / 2), 2)
                + cos($lat1) * cos($lat2) * pow(sin($dLng / 2), 2)
            )
        );

        return round($s, 3);
    }
}

/**
 * Implementation of distance calculation with Vincenty Method
 *
 * PHP version 5.3
 *
 * @category  Location
 * @package   Distance
 * @author    Marcus T. Jaschen <mjaschen@gmail.com>
 * @copyright 2013 r03.org
 * @license   http://www.opensource.org/licenses/mit-license MIT License
 * @link      http://r03.org/
 */

/**
 * Implementation of distance calculation with Vincenty Method
 *
 * @see http://www.movable-type.co.uk/scripts/latlong-vincenty.html
 *
 * @category Location
 * @package  Distance
 * @author   Marcus T. Jaschen <mjaschen@gmail.com>
 * @license  http://www.opensource.org/licenses/mit-license MIT License
 * @link     http://r03.org/
 */
class Vincenty implements DistanceInterface
{
    /**
     * @param Coordinate $point1
     * @param Coordinate $point2
     *
     * @throws NotMatchingEllipsoidException
     * @throws NotConvergingException
     *
     * @return float
     */
    public function getDistance(Coordinate $point1, Coordinate $point2)
    {
        if ($point1->getEllipsoid() != $point2->getEllipsoid()) {
            throw new NotMatchingEllipsoidException("The ellipsoids for both coordinates must match");
        }

        $lat1 = deg2rad($point1->getLat());
        $lat2 = deg2rad($point2->getLat());
        $lng1 = deg2rad($point1->getLng());
        $lng2 = deg2rad($point2->getLng());

        $a = $point1->getEllipsoid()->getA();
        $b = $point1->getEllipsoid()->getB();
        $f = 1 / $point1->getEllipsoid()->getF();

        $L  = $lng2 - $lng1;
        $U1 = atan((1 - $f) * tan($lat1));
        $U2 = atan((1 - $f) * tan($lat2));

        $iterationLimit = 100;
        $lambda         = $L;

        $sinU1 = sin($U1);
        $sinU2 = sin($U2);
        $cosU1 = cos($U1);
        $cosU2 = cos($U2);

        do {

            $sinLambda = sin($lambda);
            $cosLambda = cos($lambda);

            $sinSigma = sqrt(
                ($cosU2 * $sinLambda) * ($cosU2 * $sinLambda) +
                    ($cosU1 * $sinU2 - $sinU1 * $cosU2 * $cosLambda) * ($cosU1 * $sinU2 - $sinU1 * $cosU2 * $cosLambda)
            );

            if ($sinSigma == 0) {
                return 0.0;
            }

            $cosSigma = $sinU1 * $sinU2 + $cosU1 * $cosU2 * $cosLambda;

            $sigma = atan2($sinSigma, $cosSigma);

            $sinAlpha = $cosU1 * $cosU2 * $sinLambda / $sinSigma;

            $cosSqAlpha = 1 - $sinAlpha * $sinAlpha;

            if ($cosSqAlpha == 0) {
                $cos2SigmaM = 0;
            } else {
                $cos2SigmaM = $cosSigma - 2 * $sinU1 * $sinU2 / $cosSqAlpha;
            }

            $C = $f / 16 * $cosSqAlpha * (4 + $f * (4 - 3 * $cosSqAlpha));

            $lambdaP = $lambda;

            $lambda = $L + (1 - $C) * $f * $sinAlpha * ($sigma + $C * $sinSigma * ($cos2SigmaM + $C * $cosSigma * (- 1 + 2 * $cos2SigmaM * $cos2SigmaM)));

        } while (abs($lambda - $lambdaP) > 1e-12 && --$iterationLimit > 0);

        if ($iterationLimit == 0) {
            throw new NotConvergingException();
        }

        $uSq        = $cosSqAlpha * ($a * $a - $b * $b) / ($b * $b);
        $A          = 1 + $uSq / 16384 * (4096 + $uSq * (- 768 + $uSq * (320 - 175 * $uSq)));
        $B          = $uSq / 1024 * (256 + $uSq * (- 128 + $uSq * (74 - 47 * $uSq)));
        $deltaSigma = $B * $sinSigma * ($cos2SigmaM + $B / 4 * ($cosSigma * (- 1 + 2 * $cos2SigmaM * $cos2SigmaM) - $B / 6 * $cos2SigmaM * (- 3 + 4 * $sinSigma * $sinSigma) * (- 3 + 4 * $cos2SigmaM * $cos2SigmaM)));
        $s          = $b * $A * ($sigma - $deltaSigma);

        return (round($s, 3));
    }
}

/**
 * Ellipsoid
 *
 * PHP version 5.3
 *
 * @category  Location
 * @author    Marcus T. Jaschen <mjaschen@gmail.com>
 * @copyright 2013 r03.org
 * @license   http://www.opensource.org/licenses/mit-license MIT License
 * @link      http://www.mtb-news.de/
 */


/**
 * Ellipsoid
 *
 * @category Location
 * @author   Marcus T. Jaschen <mjaschen@gmail.com>
 * @license  http://www.opensource.org/licenses/mit-license MIT License
 * @link     http://www.mtb-news.de/
 */
class Ellipsoid
{
    /**
     * @var string
     */
    protected $name;

    /**
     * The semi-major axis
     *
     * @var float
     */
    protected $a;

    /**
     * The Inverse Flattening (1/f)
     *
     * @var float
     */
    protected $f;

    /**
     * Some often used ellipsoids
     *
     * @var array
     */
    protected static $configs = array(
        'WGS-84' => array(
            'name' => 'WGS-84',
            'a'    => 6378137.0,
            'f'    => 298.257223563,
        ),
    );

    /**
     * @param $name
     * @param $a
     * @param $f
     */
    public function __construct($name, $a, $f)
    {
        $this->name = $name;
        $this->a    = $a;
        $this->f    = $f;
    }

    /**
     * @param string $name
     *
     * @return Ellipsoid
     */
    public static function createDefault($name = 'WGS-84')
    {
        return self::createFromArray(self::$configs[$name]);
    }

    /**
     * @param $config
     *
     * @return Ellipsoid
     */
    public static function createFromArray($config)
    {
        return new self($config['name'], $config['a'], $config['f']);
    }

    /**
     * @return string
     */
    public function getName()
    {
        return $this->name;
    }

    /**
     * @return float
     */
    public function getA()
    {
        return $this->a;
    }

    /**
     * Calculation of the semi-minor axis
     *
     * @return float
     */
    public function getB()
    {
        return $this->a * (1 - 1 / $this->f);
    }

    /**
     * @return float
     */
    public function getF()
    {
        return $this->f;
    }

    /**
     * Calculates the arithmetic mean radius
     *
     * @see http://home.online.no/~sigurdhu/WGS84_Eng.html
     *
     * @return float
     */
    public function getArithmeticMeanRadius()
    {
        return $this->a * (1 - 1 / $this->f / 3);
    }
}

class NotConvergingException extends RuntimeException
{

}

class NotMatchingEllipsoidException extends InvalidArgumentException
{
    
}

/**
 * Coordinate Formatter "Decimal Degrees"
 *
 * PHP version 5.3
 *
 * @category  Location
 * @package   Formatter
 * @author    Marcus T. Jaschen <mjaschen@gmail.com>
 * @copyright 2012 r03.org
 * @license   http://www.opensource.org/licenses/mit-license MIT License
 * @link      http://r03.org/
 */

/**
 * Coordinate Formatter "Decimal Degrees"
 *
 * @category Location
 * @package  Formatter
 * @author   Marcus T. Jaschen <mjaschen@gmail.com>
 * @license  http://www.opensource.org/licenses/mit-license MIT License
 * @link     http://r03.org/
 */
class DecimalDegrees implements FormatterInterface
{
    /**
     * @var string Separator string between latitude and longitude
     */
    protected $separator;

    /**
     * @param string $separator
     */
    public function __construct($separator = " ")
    {
        $this->setSeparator($separator);
    }

    /**
     * @param Coordinate $coordinate
     *
     * @return string
     */
    public function format(Coordinate $coordinate)
    {
        return sprintf("%.5f%s%.5f", $coordinate->getLat(), $this->separator, $coordinate->getLng());
    }

    /**
     * Sets the separator between latitude and longitude values
     *
     * @param $separator
     *
     * @return $this
     */
    public function setSeparator($separator)
    {
        $this->separator = $separator;

        return $this;
    }
}

/**
 * Coordinate Formatter "DMS"
 *
 * PHP version 5.3
 *
 * @category  Location
 * @package   Formatter
 * @author    Marcus T. Jaschen <mjaschen@gmail.com>
 * @copyright 2012 r03.org
 * @license   http://www.opensource.org/licenses/mit-license MIT License
 * @link      http://r03.org/
 */


/**
 * Coordinate Formatter "DMS"
 *
 * @category Location
 * @package  Formatter
 * @author   Marcus T. Jaschen <mjaschen@gmail.com>
 * @license  http://www.opensource.org/licenses/mit-license MIT License
 * @link     http://r03.org/
 */
class DMS implements FormatterInterface
{
    const UNITS_UTF8  = 'UTF-8';
    const UNITS_ASCII = 'ASCII';

    /**
     * @var string Separator string between latitude and longitude
     */
    protected $separator;

    /**
     * Use cardinal letters for N/S and W/E instead of minus sign
     *
     * @var bool
     */
    protected $useCardinalLetters;

    /**
     * @var string
     */
    protected $unitType;

    protected $units = array(
        'UTF-8' => array(
            'deg' => '°',
            'min' => '′',
            'sec' => '″',
        ),
        'ASCII' => array(
            'deg' => '°',
            'min' => '\'',
            'sec' => '"',
        ),
    );

    /**
     * @param string $separator
     */
    public function __construct($separator = " ")
    {
        $this->setSeparator($separator);
        $this->useCardinalLetters(false);
        $this->setUnits(self::UNITS_UTF8);
    }

    /**
     * Sets the separator between latitude and longitude values
     *
     * @param $separator
     *
     * @return $this
     */
    public function setSeparator($separator)
    {
        $this->separator = $separator;

        return $this;
    }

    /**
     * @param bool $value
     *
     * @return $this
     */
    public function useCardinalLetters($value)
    {
        $this->useCardinalLetters = $value;

        return $this;
    }

    /**
     * @param string $type
     *
     * @throws \InvalidArgumentException
     */
    public function setUnits($type)
    {
        switch ($type) {
            case self::UNITS_UTF8:
                $this->unitType = self::UNITS_UTF8;
                break;
            case self::UNITS_ASCII:
                $this->unitType = self::UNITS_ASCII;
                break;
            default:
                throw new InvalidArgumentException("Invalid unit type");
        }
    }

    /**
     * @param Coordinate $coordinate
     *
     * @return string
     */
    public function format(Coordinate $coordinate)
    {
        $lat = $coordinate->getLat();
        $lng = $coordinate->getLng();

        $latValue = abs($lat);
        $latDegrees = intval($latValue);

        $latMinutesDecimal = $latValue - $latDegrees;
        $latMinutes = intval(60 * $latMinutesDecimal);

        $latSeconds = 60 * (60 * $latMinutesDecimal - $latMinutes);

        $lngValue = abs($lng);
        $lngDegrees = intval($lngValue);

        $lngMinutesDecimal = $lngValue - $lngDegrees;
        $lngMinutes = intval(60 * $lngMinutesDecimal);

        $lngSeconds = 60 * (60 * $lngMinutesDecimal - $lngMinutes);

        return sprintf(
            "%s%02d%s %02d%s %02d%s%s%s%s%03d%s %02d%s %02d%s%s",
            $this->getLatPrefix($lat),
            abs($latDegrees),
            $this->units[$this->unitType]['deg'],
            $latMinutes,
            $this->units[$this->unitType]['min'],
            round($latSeconds, 0),
            $this->units[$this->unitType]['sec'],
            $this->getLatSuffix($lat),
            $this->separator,
            $this->getLngPrefix($lng),
            abs($lngDegrees),
            $this->units[$this->unitType]['deg'],
            $lngMinutes,
            $this->units[$this->unitType]['min'],
            round($lngSeconds, 0),
            $this->units[$this->unitType]['sec'],
            $this->getLngSuffix($lng)
        );
    }

    /**
     * @param $lat
     *
     * @return string
     */
    protected function getLatPrefix($lat)
    {
        if ($this->useCardinalLetters || $lat >= 0) {
            return '';
        }

        return '-';
    }

    /**
     * @param $lng
     *
     * @return string
     */
    protected function getLngPrefix($lng)
    {
        if ($this->useCardinalLetters || $lng >= 0) {
            return '';
        }

        return '-';
    }

    /**
     * @param $lat
     *
     * @return string
     */
    protected function getLatSuffix($lat)
    {
        if (! $this->useCardinalLetters) {
            return '';
        }

        if ($lat >= 0) {
            return ' N';
        }

        return ' S';
    }

    /**
     * @param $lng
     *
     * @return string
     */
    protected function getLngSuffix($lng)
    {
        if (! $this->useCardinalLetters) {
            return '';
        }

        if ($lng >= 0) {
            return ' E';
        }

        return ' W';
    }
}

/**
 * Coordinate Formatter Interface
 *
 * PHP version 5.3
 *
 * @category  Location
 * @package   Formatter
 * @author    Marcus T. Jaschen <mjaschen@gmail.com>
 * @copyright 2012 r03.org
 * @license   http://www.opensource.org/licenses/mit-license MIT License
 * @link      http://r03.org/
 */


/**
 * Coordinate Formatter Interface
 *
 * @category Location
 * @package  Formatter
 * @author   Marcus T. Jaschen <mjaschen@gmail.com>
 * @license  http://www.opensource.org/licenses/mit-license MIT License
 * @link     http://r03.org/
 */
interface FormatterInterface
{
    /**
     * @param Coordinate $coordinate
     *
     * @return mixed
     */
    public function format(Coordinate $coordinate);
}

/**
 * GeoJSON Coordinate Formatter
 *
 * PHP version 5.3
 *
 * @category  Location
 * @package   Formatter
 * @author    Marcus T. Jaschen <mjaschen@gmail.com>
 * @copyright 2012 r03.org
 * @license   http://www.opensource.org/licenses/mit-license MIT License
 * @link      http://r03.org/
 */

/**
 * GeoJSON Coordinate Formatter
 *
 * @category Location
 * @package  Formatter
 * @author   Marcus T. Jaschen <mjaschen@gmail.com>
 * @license  http://www.opensource.org/licenses/mit-license MIT License
 * @link     http://r03.org/
 */
class GeoJSON implements FormatterInterface
{
    /**
     * @param Coordinate $coordinate
     *
     * @return string
     */
    public function format(Coordinate $coordinate)
    {
        return json_encode(
            array(
                'type'        => 'point',
                'coordinates' => array(
                    $coordinate->getLat(),
                    $coordinate->getLng(),
                ),
            )
        );
    }
}

/**
 * Line Implementation
 *
 * PHP version 5
 *
 * @category  Location
 * @author    Marcus Jaschen <mjaschen@gmail.com>
 * @copyright 1999-2013 MTB-News.de
 * @license   http://www.opensource.org/licenses/mit-license MIT License
 * @link      http://www.mtb-news.de/
 */


/**
 * Line Implementation
 *
 * @category Location
 * @author   Marcus Jaschen <mjaschen@gmail.com>
 * @license  http://www.opensource.org/licenses/mit-license MIT License
 * @link     http://www.mtb-news.de/
 */
class Line
{
    /**
     * @var \Location\Coordinate
     */
    protected $point1;

    /**
     * @var \Location\Coordinate
     */
    protected $point2;

    /**
     * @param Coordinate $point1
     * @param Coordinate $point2
     */
    public function __construct(Coordinate $point1, Coordinate $point2)
    {
        $this->point1 = $point1;
        $this->point2 = $point2;
    }

    /**
     * @param \Location\Coordinate $point1
     */
    public function setPoint1($point1)
    {
        $this->point1 = $point1;
    }

    /**
     * @return \Location\Coordinate
     */
    public function getPoint1()
    {
        return $this->point1;
    }

    /**
     * @param \Location\Coordinate $point2
     */
    public function setPoint2($point2)
    {
        $this->point2 = $point2;
    }

    /**
     * @return \Location\Coordinate
     */
    public function getPoint2()
    {
        return $this->point2;
    }

    /**
     * Calculates the length of the line (distance between the two
     * coordinates).
     *
     * @param DistanceInterface $calculator instance of distance calculation class
     *
     * @return float
     */
    public function getLength(DistanceInterface $calculator)
    {
        return $calculator->getDistance($this->point1, $this->point2);
    }
}

/**
 * Polyline Implementation
 *
 * PHP version 5
 *
 * @category  Location
 * @author    Marcus Jaschen <mjaschen@gmail.com>
 * @copyright 1999-2013 MTB-News.de
 * @license   http://www.opensource.org/licenses/mit-license MIT License
 * @link      http://www.mtb-news.de/
 */


/**
 * Polyline Implementation
 *
 * @category Location
 * @author   Marcus Jaschen <mjaschen@gmail.com>
 * @license  http://www.opensource.org/licenses/mit-license MIT License
 * @link     http://www.mtb-news.de/
 */
class Polyline
{
    /**
     * @var array
     */
    protected $points = array();

    /**
     * @param Coordinate $point
     */
    public function addPoint(Coordinate $point) {
        $this->points[] = $point;
    }

    /**
     * @return array
     */
    public function getPoints()
    {
        return $this->points;
    }

    /**
     * @return int
     */
    public function getNumberOfPoints()
    {
        return count($this->points);
    }

    /**
     * @return array
     */
    public function getSegments()
    {
        $segments = array();

        if (count($this->points) <= 1) {
            return $segments;
        }
        
        $previousPoint = $this->points[0];
        
        for ($i = 1; $i < count($this->points); $i++) {
            $point = $this->points[$i];
            $segments[] = new Line ($previousPoint, $point);
            $previousPoint = $point;
        }

//        HE: No support for reset and next without support for references. (Simplification fails)
//
//        $previousPoint = reset($this->points);
//
//        while ($point = next($this->points)) {
//            $segments[] = new Line($previousPoint, $point);
//            $previousPoint = $point;
//        }

        return $segments;
    }

    /**
     * Calculates the length of the polyline.
     *
     * @param DistanceInterface $calculator instance of distance calculation class
     *
     * @return float
     */
    public function getLength(DistanceInterface $calculator)
    {
        $distance = 0.0;

        if (count($this->points) <= 1) {
            return $distance;
        }

        $previousPoint = $this->points[0];
        
        for ($i = 1; $i < count($this->points); $i++) {
            $point = $this->points[$i];
            $distance += $calculator->getDistance($previousPoint, $point);
            $previousPoint = $point;
        }


//        HE: No support for reset and next.
//
//        $previousPoint = reset($this->points);        
// 
//        while ($point = next($this->points)) {
//            $distance += $calculator->getDistance($previousPoint, $point);
//            $previousPoint = $point;
//        }
 
        return $distance;
    }
}

/**
 * Simplify Polyline with the Douglas-Peucker-Algorithm
 *
 * The Algorithm is described here:
 * http://en.wikipedia.org/wiki/Ramer%E2%80%93Douglas%E2%80%93Peucker_algorithm
 *
 * The formula for the Perpendicular Distance is described here:
 * http://biodiversityinformatics.amnh.org/open_source/pdc/documentation.php
 *
 * PHP version 5
 *
 * @category  Location
 * @package   Processor
 * @author    Marcus Jaschen <mjaschen@gmail.com>
 * @copyright 2013 r03.org
 * @license   http://www.opensource.org/licenses/mit-license MIT License
 * @link      http://r03.org/
 */


/**
 * Simplify Polyline with the Douglas-Peucker-Algorithm
 *
 * @category Location
 * @package  Processor
 * @author   Marcus Jaschen <mjaschen@gmail.com>
 * @license  http://www.opensource.org/licenses/mit-license MIT License
 * @link     http://r03.org/
 */
class Simplify
{
    /**
     * @var \Location\Polyline
     */
    protected $polyline;

    /**
     * @param Polyline $polyline
     */
    public function __construct(Polyline $polyline)
    {
        $this->polyline = $polyline;
    }

    /**
     * @param float $tolerance The maximum allowed deviation
     *
     * @return Polyline
     */
    public function simplify($tolerance)
    {
        $simplifiedLine = $this->douglasPeucker(
            $this->polyline->getPoints(),
            $tolerance
        );

        $resultPolyline = new Polyline();

        foreach ($simplifiedLine as $point) {
            $resultPolyline->addPoint($point);
        }

        return $resultPolyline;
    }

    /**
     * @param array $line
     * @param float $tolerance
     *
     * @return array
     */
    protected function douglasPeucker($line = array(), $tolerance)
    {
        $distanceMax = 0;
        $index = 0;

        $lineSize = count($line);

        for ($i = 1; $i <= ($lineSize - 1); $i ++) {

            $distance = $this->getPerpendicularDistance($line[$i], new Line($line[0], $line[$lineSize - 1]));

            if ($distance > $distanceMax) {

                $index = $i;
                $distanceMax = $distance;

            }

        }

        if ($distanceMax > $tolerance) {

            $lineSplitFirst = array_slice($line, 0, $index);
            $lineSplitSecond = array_slice($line, $index, $lineSize);

            $recursiveResultsSplitFirst = $this->douglasPeucker($lineSplitFirst, $tolerance);
            $recursiveResultsSplitSecond = $this->douglasPeucker($lineSplitSecond, $tolerance);

            
            array_pop($recursiveResultsSplitFirst);

            $resultLine = array_merge($recursiveResultsSplitFirst, $recursiveResultsSplitSecond);

        } else {

            $resultLine = array($line[0], $line[$lineSize -  1]);

        }

        return $resultLine;
    }

    /**
     * @param Coordinate $point
     * @param Line       $line
     *
     * @return number
     */
    protected function getPerpendicularDistance(Coordinate $point, Line $line)
    {
        $ellipsoid = $point->getEllipsoid();

        $ellipsoidRadius = $ellipsoid->getArithmeticMeanRadius();

        $firstLinePointLat = $this->deg2radLatitude($line->getPoint1()->getLat());
        $firstLinePointLng = $this->deg2radLongitude($line->getPoint1()->getLng());

        $firstLinePointX = $ellipsoidRadius * cos($firstLinePointLng) * sin($firstLinePointLat);
        $firstLinePointY = $ellipsoidRadius * sin($firstLinePointLng) * sin($firstLinePointLat);
        $firstLinePointZ = $ellipsoidRadius * cos($firstLinePointLat);

        $secondLinePointLat = $this->deg2radLatitude($line->getPoint2()->getLat());
        $secondLinePointLng = $this->deg2radLongitude($line->getPoint2()->getLng());

        $secondLinePointX = $ellipsoidRadius * cos($secondLinePointLng) * sin($secondLinePointLat);
        $secondLinePointY = $ellipsoidRadius * sin($secondLinePointLng) * sin($secondLinePointLat);
        $secondLinePointZ = $ellipsoidRadius * cos($secondLinePointLat);

        $pointLat = $this->deg2radLatitude($point->getLat());
        $pointLng = $this->deg2radLongitude($point->getLng());

        $pointX = $ellipsoidRadius * cos($pointLng) * sin($pointLat);
        $pointY = $ellipsoidRadius * sin($pointLng) * sin($pointLat);
        $pointZ = $ellipsoidRadius * cos($pointLat);

        $normalizedX = $firstLinePointY * $secondLinePointZ - $firstLinePointZ * $secondLinePointY;
        $normalizedY = $firstLinePointZ * $secondLinePointX - $firstLinePointX * $secondLinePointZ;
        $normalizedZ = $firstLinePointX * $secondLinePointY - $firstLinePointY * $secondLinePointX;

        $length = sqrt($normalizedX * $normalizedX + $normalizedY * $normalizedY + $normalizedZ * $normalizedZ);

        $normalizedX /= $length;
        $normalizedY /= $length;
        $normalizedZ /= $length;

        $thetaPoint = $normalizedX * $pointX + $normalizedY * $pointY + $normalizedZ * $pointZ;

        $length = sqrt($pointX * $pointX + $pointY * $pointY + $pointZ * $pointZ);

        $thetaPoint /= $length;

        $distance = abs((M_PI / 2) - acos($thetaPoint));

        return $distance * $ellipsoidRadius;
    }

    /**
     * @param float $latitude
     *
     * @return float
     */
    protected function deg2radLatitude($latitude)
    {
        return deg2rad(90 - $latitude);
    }

    /**
     * @param float $longitude
     *
     * @return float
     */
    protected function deg2radLongitude($longitude)
    {
        if ($longitude > 0) {
            return deg2rad($longitude);
        }

        return deg2rad($longitude + 360);
    }
}

/**
 * Simple PSR-0 Autoloader
 *
 * @category  Location
 * @author    Marcus T. Jaschen <mjaschen@gmail.com>
 */
 
/* 
spl_autoload_register(
    function ($class) {
        $file = __DIR__ . '/../src/' . strtr($class, '\\', '/') . '.php';
        if (file_exists($file)) {
            require_once $file;
            return true;
        }
    }
);*/

abstract class TestCase {
    public function setup () {
        
    }
    
    public function tearDown () {
        
    }
    
    public function assertTrue ($mFound)
    {
        return $this->assertEquals (true, $mFound);
    }

    public function assertFalse ($mFound)
    {
        return $this->assertEquals (false, $mFound);
    }
    
    public function assertJsonStringEqualsJsonString ($mExpected, $mFound) 
    {
        return $this->assertEquals (json_decode ($mExpected), json_decode ($mFound));
    }
    
    public function assertCount ($mExpected, $mFound)
    {
        return $this->assertEquals ($mExpected, $mFound);
    }
    
    public function assertEquals ($mExpected, $mFound) 
    {
        if ($mExpected != $mFound) {
            echo "FAIL\n\tExpected: ";
            var_dump ($mExpected);
            echo "\tFound: ";
            var_dump ($mFound);
        } else {
            echo "PASS\n";
        }
    }
}


class BoundsTest extends TestCase
{
    /**
     * @var Bounds
     */
    protected $object;

    /**
     * Sets up the fixture, for example, opens a network connection.
     * This method is called before a test is executed.
     */
    public function setUp()
    {
        $this->object = new Bounds(
            new Coordinate(50, 10),
            new Coordinate(30, 30)
        );
    }

    /**
     * Tears down the fixture, for example, closes a network connection.
     * This method is called after a test is executed.
     */
    public function tearDown()
    {
        unset($this->object);
    }

    /**
     * @covers Location\Bounds::getNorthWest
     */
    public function testGetNorthWest()
    {
        $c = new Coordinate(50, 10);

        $this->assertEquals($c, $this->object->getNorthWest());
    }

    /**
     * @covers Location\Bounds::getSouthEast
     */
    public function testGetSouthEast()
    {
        $c = new Coordinate(30, 30);

        $this->assertEquals($c, $this->object->getSouthEast());
    }

    /**
     * @covers Location\Bounds::getNorth
     */
    public function testGetNorth()
    {
        $this->assertEquals(50, $this->object->getNorth());
    }

    /**
     * @covers Location\Bounds::getSouth
     */
    public function testGetSouth()
    {
        $this->assertEquals(30, $this->object->getSouth());
    }

    /**
     * @covers Location\Bounds::getWest
     */
    public function testGetWest()
    {
        $this->assertEquals(10, $this->object->getWest());
    }

    /**
     * @covers Location\Bounds::getEast
     */
    public function testGetEast()
    {
        $this->assertEquals(30, $this->object->getEast());
    }

    /**
     * @covers Location\Bounds::getCenter
     */
    public function testGetCenter()
    {
        $testBounds = array(
            array('nw' => new Coordinate(50, 10), 'se' => new Coordinate(30, 30), 'c' => new Coordinate(40, 20)),
            array('nw' => new Coordinate(50, - 130), 'se' => new Coordinate(30, - 110), 'c' => new Coordinate(40, - 120)),
            array('nw' => new Coordinate(10, - 10), 'se' => new Coordinate(- 10, 10), 'c' => new Coordinate(0, 0)),
            array('nw' => new Coordinate(-80, - 130), 'se' => new Coordinate(- 90, -110), 'c' => new Coordinate(-85, -120)),
            array('nw' => new Coordinate(80, - 130), 'se' => new Coordinate(90, -110), 'c' => new Coordinate(85, -120)),
            array('nw' => new Coordinate(80, 110), 'se' => new Coordinate(90, 130), 'c' => new Coordinate(85, 120)),
            array('nw' => new Coordinate(50, 170), 'se' => new Coordinate(30, -160), 'c' => new Coordinate(40, -175)),
            array('nw' => new Coordinate(-50, 150), 'se' => new Coordinate(-70, -170), 'c' => new Coordinate(-60, 170)),
        );

        foreach ($testBounds as $bounds) {

            $b = new Bounds($bounds['nw'], $bounds['se']);

            $this->assertEquals($bounds['c'], $b->getCenter());

        }
    }
}

$test = new BoundsTest ();
$test->setUp();
$test->testGetNorthWest ();
$test->tearDown();
$test->setUp();
$test->testGetSouthEast();
$test->tearDown();
$test->setUp();
$test->testGetNorth();
$test->tearDown();
$test->setUp();
$test->testGetSouth();
$test->tearDown();
$test->setUp();
$test->testGetWest();
$test->tearDown();
$test->setUp();
$test->testGetEast();
$test->tearDown();
$test->setUp();
$test->testGetCenter();
$test->tearDown();


class CoordinateTest extends TestCase
{
    /**
     * @var Ellipsoid
     */
    protected $ellipsoid;

    /**
     * @var Coordinate
     */
    protected $coordinate;

    /**
     * Sets up the fixture, for example, opens a network connection.
     * This method is called before a test is executed.
     */
    public function setUp()
    {
        $ellipsoidConfig = array(
            'name' => 'WGS-84',
            'a'    => 6378137.0,
            'f'    => 298.257223563,
        );

        $this->ellipsoid = Ellipsoid::createFromArray($ellipsoidConfig);

        $this->coordinate = new Coordinate(52.5, 13.5, $this->ellipsoid);
    }

    /**
     * Tears down the fixture, for example, closes a network connection.
     * This method is called after a test is executed.
     */
    public function tearDown()
    {
        unset($this->ellipsoid);
        unset($this->coordinate);
    }

    /**
     * @covers Location\Coordinate::__construct
     * @expectedException \InvalidArgumentException
     */
    public function testConstructorInvalidLatitude()
    {
        $c = new Coordinate('foo', 13.5, $this->ellipsoid);
    }

    /**
     * @covers Location\Coordinate::__construct
     * @expectedException \InvalidArgumentException
     */
    public function testConstructorInvalidLongitude()
    {
        $c = new Coordinate(52.5, 'foo', $this->ellipsoid);
    }

    /**
     * @covers Location\Coordinate::__construct
     */
    public function testConstructorDefaultEllipsoid()
    {
        $c = new Coordinate(52.5, 13.5);
    }

    /**
     * @covers Location\Coordinate::getLat
     */
    public function testGetLat()
    {
        $this->assertEquals(52.5, $this->coordinate->getLat());
    }

    /**
     * @covers Location\Coordinate::getLng
     */
    public function testGetLng()
    {
        $this->assertEquals(13.5, $this->coordinate->getLng());
    }

    /**
     * @covers Location\Coordinate::getEllipsoid
     */
    public function testGetEllipsoid()
    {
        $this->assertEquals($this->ellipsoid, $this->coordinate->getEllipsoid());
    }

    /**
     * @covers Location\Coordinate::getDistance
     */
    public function testGetDistance()
    {
        $coordinate1 = new Coordinate(19.820664, -155.468066, $this->ellipsoid);
        $coordinate2 = new Coordinate(20.709722, -156.253333, $this->ellipsoid);

        $this->assertEquals(128130.850, $coordinate1->getDistance($coordinate2, new Vincenty()));
    }

    /**
     * @covers Location\Coordinate::format
     */
    public function testFormat()
    {
        $this->assertEquals("52.50000 13.50000", $this->coordinate->format(new DecimalDegrees()));
    }
}



$test = new CoordinateTest ();
$test->setUp();
$test->testConstructorDefaultEllipsoid();
$test->tearDown();
$test->setUp();
$test->testGetLat();
$test->tearDown();
$test->setUp();
$test->testGetLng();
$test->tearDown();
$test->setUp();
$test->testGetEllipsoid();
$test->tearDown();
$test->setUp();
$test->testGetDistance();
$test->tearDown();
$test->setUp();
$test->testFormat();
$test->tearDown();

class HaversineTest extends TestCase
{
    protected $calculator;
    protected $ellipsoid;


    public function setUp()
    {
        $ellipsoidConfig = array(
            'name' => 'WGS-84',
            'a'    => 6378137.0,
            'f'    => 298.257223563,
        );

        $this->ellipsoid = Ellipsoid::createFromArray($ellipsoidConfig);

        $this->calculator = new Haversine();
    }


    public function tearDown()
    {
        unset($this->ellipsoid);
    }


    public function testGetDistanceZero()
    {
        $coordinate1 = new Coordinate(52.5, 13.5, $this->ellipsoid);
        $coordinate2 = new Coordinate(52.5, 13.5, $this->ellipsoid);

        $distance = $this->calculator->getDistance($coordinate1, $coordinate2);

        $this->assertEquals(0.0, $distance);
    }

    public function testGetDistanceSameLatitude()
    {
        $coordinate1 = new Coordinate(52.5, 13.5, $this->ellipsoid);
        $coordinate2 = new Coordinate(52.5, 13.1, $this->ellipsoid);

        $distance = $this->calculator->getDistance($coordinate1, $coordinate2);

        $this->assertEquals(27076.476, $distance);
    }


    public function testGetDistanceSameLongitude()
    {
        $coordinate1 = new Coordinate(52.5, 13.5, $this->ellipsoid);
        $coordinate2 = new Coordinate(52.1, 13.5, $this->ellipsoid);

        $distance = $this->calculator->getDistance($coordinate1, $coordinate2);

        $this->assertEquals(44478.032, $distance);
    }


    public function testGetDistance()
    {
        $coordinate1 = new Coordinate(19.820664, -155.468066, $this->ellipsoid);
        $coordinate2 = new Coordinate(20.709722, -156.253333, $this->ellipsoid);

        $distance = $this->calculator->getDistance($coordinate1, $coordinate2);

        $this->assertEquals(128384.515, $distance);
    }


    public function testGetDistanceInternationalDateLine()
    {
        $coordinate1 = new Coordinate(20.0, 170.0, $this->ellipsoid);
        $coordinate2 = new Coordinate(-20.0, -170.0, $this->ellipsoid);

        $distance = $this->calculator->getDistance($coordinate1, $coordinate2);

        $this->assertEquals(4952349.639, $distance);
    }


    public function testNotMatchingEllispoids()
    {
        $coordinate1 = new Coordinate(19.820664, -155.468066, $this->ellipsoid);
        $coordinate2 = new Coordinate(20.709722, -156.253333, new Ellipsoid("AnotherEllipsoid", 6378140.0, 299.2));

        $distance = $this->calculator->getDistance($coordinate1, $coordinate2);
    }
}



$test = new HaversineTest ();
$test->setUp();
$test->testGetDistanceZero();
$test->tearDown();
$test->setUp();
$test->testGetDistanceSameLatitude();
$test->tearDown();
$test->setUp();
$test->testGetDistanceSameLongitude();
$test->tearDown();
$test->setUp();
$test->testGetDistance();
$test->tearDown();
$test->setUp();
$test->testGetDistanceInternationalDateLine();
$test->tearDown();
$test->setUp();
try {
    $test->testNotMatchingEllispoids();
} catch (Exception $e) {}
$test->tearDown();

/*

class VincentyTest extends TestCase
{
    protected $ellipsoid;

    public function setUp()
    {
        $ellipsoidConfig = array(
            'name' => 'WGS-84',
            'a'    => 6378137.0,
            'f'    => 298.257223563,
        );

        $this->ellipsoid = Ellipsoid::createFromArray($ellipsoidConfig);
    }


    public function tearDown()
    {
        unset($this->ellipsoid);
    }


    public function testGetDistanceZero()
    {
        $coordinate1 = new Coordinate(52.5, 13.5, $this->ellipsoid);
        $coordinate2 = new Coordinate(52.5, 13.5, $this->ellipsoid);

        $calculator = new Vincenty();
        $distance = $calculator->getDistance($coordinate1, $coordinate2);

        $this->assertEquals(0.0, $distance);
    }


    public function testGetDistanceSameLatitude()
    {
        $coordinate1 = new Coordinate(52.5, 13.5, $this->ellipsoid);
        $coordinate2 = new Coordinate(52.5, 13.1, $this->ellipsoid);

        $calculator = new Vincenty();
        $distance = $calculator->getDistance($coordinate1, $coordinate2);

        $this->assertEquals(27164.059, $distance);
    }


    public function testGetDistanceSameLongitude()
    {
        $coordinate1 = new Coordinate(52.5, 13.5, $this->ellipsoid);
        $coordinate2 = new Coordinate(52.1, 13.5, $this->ellipsoid);

        $calculator = new Vincenty();
        $distance = $calculator->getDistance($coordinate1, $coordinate2);

        $this->assertEquals(44509.218, $distance);
    }


    public function testGetDistance()
    {
        $coordinate1 = new Coordinate(19.820664, -155.468066, $this->ellipsoid);
        $coordinate2 = new Coordinate(20.709722, -156.253333, $this->ellipsoid);

        $calculator = new Vincenty();
        $distance = $calculator->getDistance($coordinate1, $coordinate2);

        $this->assertEquals(128130.850, $distance);
    }


    public function testGetDistanceInternationalDateLine()
    {
        $coordinate1 = new Coordinate(20.0, 170.0, $this->ellipsoid);
        $coordinate2 = new Coordinate(-20.0, -170.0, $this->ellipsoid);

        $calculator = new Vincenty();
        $distance = $calculator->getDistance($coordinate1, $coordinate2);

        $this->assertEquals(4932842.135, $distance);
    }


    public function testNotMatchingEllispoids()
    {
        $coordinate1 = new Coordinate(19.820664, -155.468066, $this->ellipsoid);
        $coordinate2 = new Coordinate(20.709722, -156.253333, new Ellipsoid("AnotherEllipsoid", 6378140.0, 299.2));

        $calculator = new Vincenty();
        $distance = $calculator->getDistance($coordinate1, $coordinate2);
    }
}


$test = new VincentyTest ();
$test->setUp();
$test->testGetDistanceZero();
$test->tearDown();
$test->setUp();
$test->testGetDistanceSameLatitude();
$test->tearDown();
$test->setUp();
$test->testGetDistanceSameLongitude();
$test->tearDown();
$test->setUp();
$test->testGetDistance();
$test->tearDown();
$test->setUp();
$test->testGetDistanceInternationalDateLine();
$test->tearDown();
$test->setUp();
try {
    $test->testNotMatchingEllispoids();
} catch (Exception $e) {}
$test->tearDown();

class DecimalDegreesTest extends TestCase
{
    protected $object;

    public function setUp()
    {
        $this->object = new DecimalDegrees;
    }

    public function tearDown()
    {
    }


    public function testFormatDefaultSeparator()
    {
        $coordinate = new Coordinate(52.5, 13.5);

        $formatter = new DecimalDegrees();

        $this->assertEquals("52.50000 13.50000", $formatter->format($coordinate));
    }


    public function testFormatCustomSeparator()
    {
        $coordinate = new Coordinate(52.5, 13.5);

        $formatter = new DecimalDegrees(", ");

        $this->assertEquals("52.50000, 13.50000", $formatter->format($coordinate));
    }
}

$test = new DecimalDegreesTest ();
$test->setUp();
$test->testFormatDefaultSeparator();
$test->tearDown();
$test->setUp();
$test->testFormatCustomSeparator();
$test->tearDown();


class DMSTest extends TestCase
{
    protected $formatter;


    public function setUp()
    {
        $this->formatter = new DMS;
    }


    public function tearDown()
    {
    }


    public function testFormatDefaultSeparator()
    {
        $coordinate = new Coordinate(52.5, 13.5);

        $this->assertEquals("52° 30′ 00″ 013° 30′ 00″", $this->formatter->format($coordinate));
    }

    public function testFormatCustomSeparator()
    {
        $coordinate = new Coordinate(18.911306, -155.678268);

        $this->formatter->setSeparator(", ");

        $this->assertEquals("18° 54′ 41″, -155° 40′ 42″", $this->formatter->format($coordinate));
    }


    public function testFormatCardinalLetters()
    {
        $coordinate = new Coordinate(18.911306, -155.678268);

        $this->formatter->setSeparator(", ")->useCardinalLetters(true);

        $this->assertEquals("18° 54′ 41″ N, 155° 40′ 42″ W", $this->formatter->format($coordinate));
    }

    public function testFormatBothNegative()
    {
        $coordinate = new Coordinate(-18.911306, -155.678268);

        $this->formatter->setSeparator(", ");

        $this->assertEquals("-18° 54′ 41″, -155° 40′ 42″", $this->formatter->format($coordinate));
    }


    public function testFormatASCIIUnits()
    {
        $coordinate = new Coordinate(-18.911306, -155.678268);

        $this->formatter->setSeparator(", ")->setUnits(DMS::UNITS_ASCII);

        $this->assertEquals("-18° 54' 41\", -155° 40' 42\"", $this->formatter->format($coordinate));
    }
}



$test = new DMSTest ();
$test->setUp();
$test->testFormatDefaultSeparator();
$test->tearDown();
$test->setUp();
$test->testFormatCustomSeparator();
$test->tearDown();
$test->setUp();
$test->testFormatCardinalLetters();
$test->tearDown();
$test->setUp();
$test->testFormatBothNegative();
$test->tearDown();
$test->setUp();
$test->testFormatASCIIUnits();
$test->tearDown();

class GeoJSONTest extends TestCase
{

    protected $formatter;


    public function setUp()
    {
        $this->formatter = new GeoJSON;
    }


    public function tearDown()
    {
    }


    public function testFormatDefault()
    {
        $coordinate = new Coordinate(52.5, 13.5);

        $json = '{ "type" : "point" , "coordinates" : [ 52.5, 13.5 ] }';

        $this->assertJsonStringEqualsJsonString($json, $this->formatter->format($coordinate));
    }

    public function testFormatPrecision()
    {
        $coordinate = new Coordinate(52.123456789012345, 13.123456789012345);

        $json = '{ "type" : "point" , "coordinates" : [ 52.123456789012345, 13.123456789012345 ] }';

        $this->assertJsonStringEqualsJsonString($json, $this->formatter->format($coordinate));
    }
}

$test = new GeoJSONTest ();
$test->setUp();
$test->testFormatDefault();
$test->tearDown();
$test->setUp();
$test->testFormatPrecision();
$test->tearDown();


class LineTest extends TestCase
{
    public function testCreateLine()
    {
        $point1 = new Coordinate(52.5, 13.5);
        $point2 = new Coordinate(64.1, -21.9);

        $line = new Line($point1, $point2);

        $this->assertEquals($point1, $line->getPoint1());
        $this->assertEquals($point2, $line->getPoint2());
    }

    public function testCalculateLength()
    {
        $point1 = new Coordinate(52.5, 13.5);
        $point2 = new Coordinate(64.1, -21.9);

        $line = new Line($point1, $point2);

        $this->assertEquals(2397867.8, $line->getLength(new Vincenty()), '', 0.01);
    }
}

$test = new LineTest ();
$test->testCreateLine ();
$test->testCalculateLength();

class PolylineTest extends TestCase
{
    protected $polyline;

    public function setUp()
    {
        $this->polyline = new Polyline();
        $this->polyline->addPoint(new Coordinate(52.5, 13.5));
        $this->polyline->addPoint(new Coordinate(64.1, -21.9));
        $this->polyline->addPoint(new Coordinate(40.7, -74.0));
        $this->polyline->addPoint(new Coordinate(33.9, -118.4));
    }

    public function testCreatePolyline()
    {
        $this->assertCount(4, $this->polyline->getPoints());
    }

    public function testGetSegments()
    {
        $segments = $this->polyline->getSegments();

        $this->assertEquals(new Line(new Coordinate(52.5, 13.5), new Coordinate(64.1, -21.9)), $segments[0]);
        $this->assertEquals(new Line(new Coordinate(64.1, -21.9), new Coordinate(40.7, -74.0)), $segments[1]);
        $this->assertEquals(new Line(new Coordinate(40.7, -74.0), new Coordinate(33.9, -118.4)), $segments[2]);
    }

    public function testGetLength()
    {
        $this->assertEquals(10576798.9, $this->polyline->getLength(new Vincenty()), '', 0.1);
    }
}

$test = new PolylineTest();
$test->setUp();
$test->testCreatePolyline();
$test->setUp();
$test->testGetSegments();
$test->setUp();
$test->testGetLength ();

class SimplifyTest extends TestCase
{
    public function testSimplifyThreePointsToTwoPoints()
    {
        $polyline = new Polyline();
        $polyline->addPoint(new Coordinate(10.0, 10.0));
        $polyline->addPoint(new Coordinate(20.0, 20.0));
        $polyline->addPoint(new Coordinate(30.0, 10.0));

        $processor = new Simplify($polyline);

        // actual deviation is 1046 km, so 1500 km is enough of tolerance to strip the 2nd coordinate
        $simplified = $processor->simplify(1500000);
        
        $segments = $simplified->getSegments();
        
        $this->assertEquals(1, count($segments));
        $this->assertEquals(new Line(new Coordinate(10.0, 10.0), new Coordinate(30.0, 10.0)), $segments[0]);
    }

    public function testSimplifyThreePointsImpossible()
    {
        $polyline = new Polyline();
        $polyline->addPoint(new Coordinate(10.0, 10.0));
        $polyline->addPoint(new Coordinate(20.0, 20.0));
        $polyline->addPoint(new Coordinate(30.0, 10.0));

        $processor = new Simplify($polyline);

        $simplified = $processor->simplify(1000);

        $segments = $simplified->getSegments();

        $this->assertEquals($polyline, $simplified);
    }
}

$test = new SimplifyTest ();
$test->testSimplifyThreePointsToTwoPoints();
$test->testSimplifyThreePointsImpossible();

*/

?>