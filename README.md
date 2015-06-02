# mirage-stats-demo

Unikernel that displays stats on a web page. Currently very basic and experimental.

### Instructions

```
mirage configure --xen
make depend
make
# edit www.xl to uncomment the vif line
sudo xl create -c www.xl
```

You can select a given IP address and gateways:

```
IP=<ip> GW=<ip> mirage configure --xen
```
