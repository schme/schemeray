Requires ChezScheme (only one tested) with threads enabled

```bash
git clone git@github.com:cisco/ChezScheme.git 
cd ChezScheme
./configure --threads
sudo make install
```

After that, a `./main.scm` or `scheme main.scm` to run.

`raytracer.scm` contains the scene and rendering settings.

## Improvements
- Make vec3 a record instead of a list. Even though it's fun to be able to call the functional
algorithms for most of the operations, it's pretty louzy for performance
- Separate the configuration/scene to a different file that can be loaded
  - Make that configuration file super awesome with runnable materials and other nice things
