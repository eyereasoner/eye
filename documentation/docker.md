![EYE](https://josd.github.io/images/eye.png)

# Running EYE using Docker

EYE can be run using a Docker installation.

Pull the latest version of eye:

```
docker pull eyereasoner/eye
```

## Linux/OSX

Run the EYE reasoner:

```
docker run --net=host --rm -v $HOME:$HOME -w $(pwd) -ti eyereasoner/eye --help
```

Use the EYE reasoner with a local Notation3 file as input:

```
docker run --net=host --rm -v $HOME:$HOME -w $(pwd) -ti eyereasoner/eye --nope --quiet --pass myfile.n3
```

## Windows

Run the EYE reasoner:

```
docker run --net=host --rm -v %HOMEPATH%:%HOMEPATH% -w %cd% -ti eyereasoner/eye --help
```

Use the EYE reasoner with a local Notation3 file as input:

```
docker run --net=host --rm -v %HOMEPATH%:%HOMEPATH% -w %cd% -ti eyereasoner/eye --nope --quiet --pass myfile.n3
```




