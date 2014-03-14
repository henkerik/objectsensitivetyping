<?php

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

$e = Ellipsoid::createDefault();
$x = $e;

?>