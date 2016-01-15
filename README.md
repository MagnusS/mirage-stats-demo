# mirage-stats-demo

Unikernel that displays stats on a web page. Currently very basic and
experimental. 

A live version is hosted with [Jitsu](http://github.com/mirage/jitsu.git) at [jitsu.v0.no](http://jitsu.v0.no).

### Instructions

To compile the app into a Xen image:

```
mirage configure --xen
make depend
make
# edit www.xl to uncomment the vif line
sudo xl create -c www.xl
```

By default, it will use IP=10.0.0.2 and GW=10.0.0.1. You can change these
settings by doing:

```
IP=<ip> GW=<ip> mirage configure --xen
```
