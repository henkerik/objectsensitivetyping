<?php

class Camera {
  protected $position;
  protected $direction;
  protected $angle;
  protected $up;
  protected $right;

  public function __construct() {
    $this->position = new Vector(0, 0, 0);

    // Assume that up is always somewhere in the <0, 1, 0> <direction> plane
    $this->up = new Vector(0, 1, 0);

    $this->angle = 30 / 180 * M_PI;
  }

  public function getDirection() {
    return $this->direction;
  }

  public function getAngle() {
    return $this->angle;
  }

  public function getRight() {
    return $this->right;
  }

  public function getUp() {
    return $this->up;
  }

  public function setPosition(Vector $v) {
    $this->position = $v;
    return $this;
  }

  public function getPosition() {
    return $this->position;
  }

  public function setLookAt(Vector $v) {
    // Compute direction (<look at> - <position>)
    $this->direction = $v->klone();
    $this->direction->V_sub($this->position);

    my_assert(!$this->direction->isNull(), 'look at == position');

    // Normalize
    $this->direction->normalize();

    // Compute <right>
    $this->right = $this->up->klone();
    $this->right->V_cross($this->direction);

    // Compute the real <up>
    $this->up = $this->direction->klone();
    $this->up->V_cross($this->right);

    return $this;
  }
}

class Color extends Vector {
  public static $white;
  public static $black;
  public static $red;
  public static $green;
  public static $blue;

  public function __construct($x, $y, $z) {
    $x = min($x, 1);
    $y = min($y, 1);
    $z = min($z, 1);

    parent::__construct($x, $y, $z);
  }
  
  public function klone ()
  {
      return new Color ($this->x, $this->y, $this->z);
  }

  public function toInt() {
    return (($this->x * 255 & 0xff) << 16) |
           (($this->y * 255 & 0xff) << 8) |
           ($this->z * 255 & 0xff);
  }

  public function __toString() {
    return sprintf('%02x%02x%02x', $this->x * 255 & 0xff, $this->y * 255 & 0xff, $this->z * 255 & 0xff);
  }
}

Color::$white = new Color(1, 1, 1);
Color::$black = new Color(0, 0, 0);
Color::$red   = new Color(1, 0, 0);
Color::$green = new Color(0, 1, 0);
Color::$blue  = new Color(0, 0, 1);

/**
 * Poor man's BMP library.
 *
 * This code creates uncompressed BMP files. I'm not sure it's 100% compliant to the spec,
 * but it seems to render fine.
 */

class BMPEncoder extends Encoder {
  protected $width;
  protected $height;

  protected $pixels = array();

  public function __construct($width, $height) {
    $this->width = $width;
    $this->height = $height;

    for ($j = 0; $j < $height; $j++) {
      for ($i = 0; $i < $width; $i++) {
        $this->setPixel($i, $j, Color::$black);
      }
    }
  }

  public function setPixel($x, $y, Color $color) {
    $this->pixels[$x + $y * $this->width] = $color;
  }

  public function writeFile($file) {
    $file = $file . '.bmp';
    $fh = fopen($file, 'w');
    if (!$fh) {
      echo 'Failed to write to ', $this->file, "\n";
      return;
    }

    fwrite($fh, pack('c*',
                     0x42, 0x4d)                            // "BM"
                    );
    fwrite($fh, pack('V*',
                     $this->width * $this->height * 3 + 0x36)  // total file size
                    );
    fwrite($fh, pack('c*',
                     0x00, 0x00, 0x00, 0x00,                // reserved
                     0x36, 0x00, 0x00, 0x00,                // BOF
                     40,   0x00, 0x00, 0x00)                // no BitMapInfoHeader
                    );
    fwrite($fh, pack('V*',
                     $this->width,
                     $this->height)
                    );
    fwrite($fh, pack('c*',
                     0x01, 0x00,                            // number of planes
                     24,   0x00,                            // number of bits per pixel (24)
                     0x00, 0x00, 0x00, 0x00)                // compression type
                    );
    fwrite($fh, pack('V*',
                     $this->width * $this->height * 3)      // size of bitmap
                    );
    fwrite($fh, pack('c*',
                     0x00, 0x00, 0x00, 0x00,                // horizontal resolution
                     0x00, 0x00, 0x00, 0x00,                // vertical resolution
                     0x00, 0x00, 0x00, 0x00,                // number of used colors
                     0x00, 0x00, 0x00, 0x00)                // number of important colors
                    );

    // Dump every pixel
    for ($j = $this->height-1; $j >= 0; $j--) {
      for ($i = 0; $i < $this->width; $i++) {
        $c = $this->pixels[$i + $j * $this->width];
        fwrite($fh, pack('c*', $c->z * 255, $c->y * 255, $c->x * 255));
      }
    }

    fclose($fh);
    echo 'wrote ', $file, "\n";
  }
}

/**
 * I have abstracted out the encoder (the piece of code which generates a bmp, png, jpg or
 * whatever file format you want), so that things work even if GD is not installed.
 *
 * It is however recommended to use GD, because it's significantly faster.
 */
abstract class Encoder {
  abstract function __construct($width, $height);
  abstract function setPixel($x, $y, Color $color);
  abstract function writeFile($file);
}


class GDEncoder extends Encoder {
  protected $img = null;

  public function __construct($width, $height) {
    $this->img = imagecreatetruecolor($width, $height);
  }

  public function setPixel($x, $y, Color $color) {
    imagesetpixel($this->img, $x, $y, $color->toInt());
  }

  public function writeFile($file) {
    $file = $file.'.png';
    imagepng($this->img, $file);
    echo 'wrote ', $file, "\n";
  }
}

/**
 * Abstract light
 */
abstract class Light {

}

/**
 * A point light is a light which is located at given point and shines in all directions
 * with infinite power. This results in hard shadows.
 */
class PointLight extends Light {
  protected $position;

  public function __construct() {
    $this->position = new Vector(0, 0, 0);
  }

  public function setPosition(Vector $v) {
    $this->position = $v;
    return $this;
  }

  public function getPosition() {
    return $this->position;
  }
}


abstract class Object {
  protected $name;
  protected $position;
  protected $color;

  public function __construct($name) {
    $this->name = $name;
    $this->position = new Vector(0, 0, 0);
    $this->color = Color::$white;
  }

  public function getName() {
    return $this->name;
  }

  public function setPosition(Vector $v) {
    $this->position = $v;
    return $this;
  }

  public function getPosition() {
    return $this->position;
  }

  public function setColor(Color $c) {
    $this->color = $c;
    return $this;
  }

  public function getColor() {
    return $this->color;
  }

  /**
   * Calculates intersection between the Ray $r and this object.
   *
   * Returns null if the ray does not intersect the object. If it
   * does, we return an array(
   *   'd' => distance from ray's origin,
   *   'p' => point of intersection (if $compute_point is true),
   *   'n' => normal vector (if $compute_normal is true)
   * )
   */
  abstract function intersect(Ray $r, $compute_point, $compute_normal);
}


class Plane extends Object {
  protected $normal;

  public function setNormal(Vector $n) {
    $this->normal = $n->klone();
    $this->normal->normalize();
    return $this;
  }

  public function intersect(Ray $ray, $compute_point, $compute_normal) {
    $d = -Vector::dot($this->position, $this->normal);
    $denom = Vector::dot($this->normal, $ray->getDirection());
    if ($denom == 0) {
      return null;
    }
    $num = -$d - Vector::dot($this->normal, $ray->getOrigin());
    $t = $num / $denom;

    if ($t < 0) {
      return null;
    }

    $r = array('d' => $t);

    if ($compute_point || $compute_normal) {
      $t2 = $ray->getDirection()->klone();
      $t2->K_mul($t);
      $t2->V_add($ray->getOrigin());

      $r['p'] = $t2;

      $r['n'] = $this->normal;
    }
    return $r;
  }
}

class Sphere extends Object {
  protected $radius = 1;

  public function intersect(Ray $ray, $compute_point, $compute_normal) {
    // A sphere-ray intersection can happen in 0, 1 or 2 points
    $dst = $ray->getOrigin()->klone();
    $dst->V_sub($this->position);

    $b = Vector::dot($dst, $ray->getDirection());
    $c = Vector::dot($dst, $dst) - $this->radius * $this->radius;
    $d = $b * $b - $c;
    if ($d < 0) {
      return null;
    }

    $t = -$b - sqrt($d);
    if ($t < 0) {
      return null;
    }

    $r = array('d' => $t);

    if ($compute_point || $compute_normal) {
      $t2 = $ray->getDirection()->klone();
      $t2->K_mul($t);
      $t2->V_add($ray->getOrigin());

      $r['p'] = $t2;
    }

    if ($compute_normal) {
      $n = $this->position->klone();
      $n->neg();
      $n->V_add($t2);
      $n->normalize();

      $r['n'] = $n;
    }
    return $r;
  }

  public function setRadius($r) {
    $this->radius = (float)$r;
    return $this;
  }
}



class Ray {
  protected $origin;
  protected $direction;

  public function setOrigin(Vector $v) {
    $this->origin = $v;
    return $this;
  }

  public function getOrigin() {
    return $this->origin;
  }

  public function setDirection(Vector $v) {
    $this->direction = $v->klone();
    $this->direction->normalize();

    return $this;
  }

  public function getDirection() {
    return $this->direction;
  }

  public static function reflectedRay(Ray $i, Vector $n, Vector $p) {
    $r = new Ray();
    $r->origin = $p;
    $r->direction = Vector::reflectedVector($i->getDirection(), $n);
    return $r;
  }

  public static function dot(Ray $r1, Ray $r2) {
    return Vector::dot($r1->getDirection(), $r2->getDirection());
  }
}


/**
 * Diffuse rendering involves looking at the angle between a ray and the surface's normal to calculate
 * the amount of color to render.
 */

class DiffuseRenderer extends Renderer {
  protected function render_ray(World $world, Ray $ray, $ignore, $recursion) {
    $r = $this->rayIntersection($world, $ray, $ignore, true, true);
    if (!$r) {
      // ray does not intersect any object
      return;
    }

    $light_ray = $this->pointLight($world, $r['p'], $r['o']);
    if (!$light_ray) {
      // point is not exposed to any light sources
      return;
    }

    // Calculate pixel's color
    $shading = max(Vector::dot($light_ray->getDirection(), $r['n']), 0);
    $c = $r['o']->getColor()->klone();
    $c->K_mul($shading);

    return $c;
  }
}


class FlatRenderer extends Renderer {
  protected function render_ray(World $world, Ray $ray, $ignore, $recursion) {
    $r = $this->rayIntersection($world, $ray, $ignore, true, false);
    if (!$r) {
      // ray does not intersect any object
      return;
    }

    $light_ray = $this->pointLight($world, $r['p'], $r['o']);
    if (!$light_ray) {
      // point is not exposed to any light sources
      return;
    }

    return $r['o']->getColor();
  }
}


/**
 * Phong (specular) rendering involves looking at the angle between the ray and the light.
 * This makes objects look shiny.
 */

class PhongRenderer extends Renderer {
  protected function render_ray(World $world, Ray $ray, $ignore, $recursion) {
    $r = $this->rayIntersection($world, $ray, $ignore, true, true);
    if (!$r) {
      // ray does not intersect any object
      return null;
    }

    $light_ray = $this->pointLight($world, $r['p'], $r['o']);
    if (!$light_ray) {
      // object is not exposed to any lights
      return null;
    }

    // Calculate pixel's color
    $diffuse_shading = max(Vector::dot($light_ray->getDirection(), $r['n']), 0);

    $reflected_ray = Ray::reflectedRay($ray, $r['n'], $r['p']);

    $specular_shading = max(Ray::dot($light_ray, $reflected_ray), 0);
    $specular_shading = pow($specular_shading, 16);

    $reflection_shading = null;
    if ($this->reflections && ($recursion < 3)) {
      if ($r['o']->getName() == 'white sphere') {
        $reflection_shading = $this->render_ray($world, $reflected_ray, $r['o'], $recursion+1);
      }
    }
    $total = 0.7 * $diffuse_shading + 0.3 * $specular_shading;
    $c = $r['o']->getColor()->klone();
    $c->K_mul(min($total, 1));

    if (($recursion == 1) && $reflection_shading) {
      $c->K_mul(0.2);
      $c2 = $reflection_shading->klone();
      $c2->K_mul(0.8);
      $c->V_add($c2);
      return $c;
    } else {
      return $c;
    }
  }
}


abstract class Renderer {
  protected $anti_alias = false;
  protected $reflections = false;

  public function setAntiAlias($bool) {
    $this->anti_alias = $bool;
    return $this;
  }

  public function setReflection($bool) {
    $this->reflections = $bool;
    return $this;
  }

  /**
   *  Find the closest object in $world that $ray intersects.
   *
   *  Returns an array(
   *    'o' => object which intersected
   *    'd' => distance
   *    'p' => point of intersection
   *    'n' => normal vector
   *  );
   *
   *  or null if the ray does not intersect anything.
   */
  protected function rayIntersection(World $world, Ray $ray, $ignore, $compute_point, $compute_normal) {
    $result = null;
    foreach ($world->getObjects() as $obj) {
      if ($obj === $ignore) {
        continue;
      }
      $t = $obj->intersect($ray, $compute_point, $compute_normal);
      if ($t === null) {
        continue;
      }
      if (!$result || ($t['d'] < $result['d'])) {
        $result = $t;
        $result['o'] = $obj;
      }
    }
    return $result;
  }

  protected function pointLight(World $world, Vector $point, Object $ignore) {
    $ray = new Ray();
    $ray->setOrigin($point);
    foreach ($world->getLights() as $light) {
      $ray->setDirection(Vector::fromAtoB(
        $point,
        $light->getPosition()));
      $hits_light = false;
      foreach ($world->getObjects() as $obj) {
        if ($obj === $ignore) {
          continue;
        }
        if ($obj->intersect($ray, false, false) !== null) {
          $hits_light = true;
          break;
        }
      }
      if (!$hits_light) {
        return $ray;
      }
    }
    return null;
  }

  function render(World $world, Encoder $img, $width, $height) {
    $camera = $world->getCamera();

    $camera_z = $camera->getDirection()->klone();
    $camera_z->K_mul($width / 2 / tan($camera->getAngle()));

    // Cast rays, ($i, $j) is screen coordinates
    for ($j = 0; $j < $height; $j++) {
      for ($i = 0; $i < $width; $i++) {
        // Rays start at <camera> and go to
        // (d * <direction>) + (i - width/2) * <right>) + (+height/2 - j) * up
        $r = $camera_z->klone();

        $new_i = $i;
        $new_j = $j;
        if ($this->anti_alias) {
          $new_i += mt_rand() / mt_getrandmax();
          $new_j += mt_rand() / mt_getrandmax();
        }

        $t = $camera->getRight()->klone();
        $t->K_mul($new_i - $width / 2);
        $r->V_add($t);

        $t = $camera->getUp()->klone();
        $t->K_mul($height / 2 - $new_j);
        $r->V_add($t);

        $ray = new Ray();
        $ray->setOrigin($camera->getPosition());
        $ray->setDirection($r);

        $c = $this->render_ray($world, $ray, null, 1);
        if ($c) {
          $img->setPixel($i, $j, $c);
        }
      }
    }
  }

  /**
   * Abstract ray rendering function will be implemented in the various renderers.
   *
   * Returns a color for the ray or null if the ray doesn't intersect any object.
   */
  abstract protected function render_ray(World $world, Ray $ray, $ignore, $recursion);
}


/**
 * Simplest form of rendering. This rendering does not take into account
 * any shadows or lights. It simply projects rays and considers every
 * object to be totally lit.
 */

class SimpleRenderer extends Renderer {
  protected function render_ray(World $world, Ray $ray, $ignore, $recursion) {
    $r = $this->rayIntersection($world, $ray, $ignore, false, false);
    if (!$r) {
      // ray does not intersect any object
      return;
    }

    return $r['o']->getColor();
  }
}


function my_assert($bool, $message='') {
  if (!$bool) {
    die($message);
  }
}

function id($obj) {
  return $obj;
}


class Vector {
  public $x;
  public $y;
  public $z;

  public function __construct($x, $y, $z) {
    $this->x = (float)$x;
    $this->y = (float)$y;
    $this->z = (float)$z;
  }
  
  public function klone ()
  {
      return new Vector ($this->x, $this->y, $this->z);
  }

  public static function fromAtoB(Vector $a, Vector $b) {
    $r = $b->klone();
    $r->V_sub($a);
    return $r;
  }

  public function equals(Vector $v2) {
    return (($this->x == $v2->x) &&
            ($this->y == $v2->y) &&
            ($this->z == $v2->z));
  }

  public function length() {
    return sqrt($this->x * $this->x +
                $this->y * $this->y +
                $this->z * $this->z);
  }

  public function normalize() {
    $this->K_div($this->length());
  }

  public function assertNormalized() {
    $l = $this->length();
    assert(($l >= 0.99) && ($l <= 1.01));
  }

  public function neg() {
    $this->x = -$this->x;
    $this->y = -$this->y;
    $this->z = -$this->z;
  }

  public function V_add(Vector $v2) {
    $this->x = $this->x + $v2->x;
    $this->y = $this->y + $v2->y;
    $this->z = $this->z + $v2->z;

    return $this;
  }

  public function V_sub(Vector $v2) {
    $this->x = $this->x - $v2->x;
    $this->y = $this->y - $v2->y;
    $this->z = $this->z - $v2->z;

    return $this;
  }

  /**
   * Returns the dot product of two vectors. This is
   * also cos(alpha) * ||v1|| * ||v2||, where
   * alpha is the angle between v1 and v2.
   *
   * @return float
   */
  public static function dot(Vector $v1, Vector $v2) {
    return ($v1->x * $v2->x + $v1->y * $v2->y + $v1->z * $v2->z);
  }

  public function V_cross(Vector $v2) {
    $i = $this->y * $v2->z - $this->z * $v2->y;
    $j = $this->z * $v2->x - $this->x * $v2->z;
    $z = $this->x * $v2->y - $this->y * $v2->x;

    $this->x = $i;
    $this->y = $j;
    $this->z = $z;

    return $this;
  }

  public function K_mul($k) {
    $this->x = $this->x * $k;
    $this->y = $this->y * $k;
    $this->z = $this->z * $k;
    return $this;
  }

  public function K_div($k) {
    $this->x = $this->x / $k;
    $this->y = $this->y / $k;
    $this->z = $this->z / $k;
    return $this;
  }

  /**
   * @return bool
   */
  public function isNull() {
    return (($this->x == 0) &&
            ($this->y == 0) &&
            ($this->z == 0));
  }

  /**
   * Given an incident vector and normal vector, a reflected
   * vector is returned.
   *
   * The usual formula is:
   * r = i - 2 * i.n * n
   */
  public static function reflectedVector(Vector $i, Vector $n) {
    $i->assertNormalized();
    $n->assertNormalized();

    $r = $i->klone();
    $t = $n->klone();
    $t->K_mul(-2 * Vector::dot($r, $t));
    $r->V_add($t);
    $r->assertNormalized();
    return $r;
  }
}

/**
 * The World object. This object is a basically a container. It doesn't
 * do much, beside tie everything together.
 *
 * A World is composed of:
 * - exactly one camera
 * - one or more lights
 * - zero or more objects
 * - exactly one rendering engine
 */
class World {
  protected $camera = null;
  protected $lights = array();
  protected $objects = array();
  protected $renderer = null;

  public function __construct() {
    ini_set('memory_limit', '2000M');
    // So that each run returns the exact same image
    mt_srand(0);
  }

  // Sets the camera for the world. Each world should
  // have exactly one camera before rendering can happen
  public function setCamera(Camera $camera) {
    if ($this->camera) {
      throw new Exception('Camera already set');
    }
    $this->camera = $camera;
    return $this;
  }

  public function getCamera() {
    return $this->camera;
  }

  public function setRenderer(Renderer $renderer) {
    if ($this->renderer) {
      throw new Exception('Renderer already set');
    }
    $this->renderer = $renderer;
    return $this;
  }

  // Adds a light source to the world. A world should have
  // one or more lights.
  public function addLight(Light $light) {
    $this->lights[] = $light;
    return $this;
  }

  public function getLights() {
    return $this->lights;
  }

  public function addObject(Object $obj) {
    $this->objects[] = $obj;
    return $this;
  }

  public function getObjects() {
    return $this->objects;
  }

  public function render($file, $width=400, $height=225) {
    if (!$this->camera) {
      throw new Exception('You need to set a Camera');
    }
    if (!$this->lights) {
      throw new Exception('You need one or more Lights');
    }
    if (!$this->renderer) {
      throw new Exception('You need to set a Renderer');
    }

    if (function_exists('gd_info')) {
      $img = new GDEncoder($width, $height);
    } else {
      $img = new BMPEncoder($width, $height);
    }
    $this->renderer->render($this, $img, $width, $height);
    $img->writeFile($file);
  }
}



/**
 * A simple world, with 2 spheres and a plane.
 * Rendered using Phong rendering and anti aliasing.
 */

$camera = id(new Camera())
  ->setPosition(new Vector(10, 30, -100))
  ->setLookAt(new Vector(0, 10, 0));

$light = id(new PointLight())
  ->setPosition(new Vector(100, 100, -100));

$sphere = id(new Sphere('red sphere'))
  ->setPosition(new Vector(0, 0, 0))
  ->setRadius(10)
  ->setColor(Color::$red);

$sphere2 = id(new Sphere('green sphere'))
  ->setPosition(new Vector(0, 18, 0))
  ->setRadius(10)
  ->setColor(Color::$green);

$plane = id(new Plane('floor'))
  ->setPosition(new Vector(0, -10, 0))
  ->setNormal(new Vector(0, 1, 0))
  ->setColor(Color::$blue);

$renderer = id(new PhongRenderer())
  ->setAntiAlias(true);

$world = id(new World())
  ->setCamera($camera)
  ->addObject($sphere)
  ->addObject($sphere2)
  ->addObject($plane)
  ->addLight($light)
  ->setRenderer($renderer);

$world->render('sample', 10, 20);

?>