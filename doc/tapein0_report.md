## Tape-In 0 Report

The following items are in tape-in 0:

### Harrison
- Created two FSMs, one for channel estimation using FFTs (BPSK OFDM pilots) and the other using Golay codewords.
- Unit tests are complete for FixedPoint versions of both, DspReal works for Golay.
- Unit test stimuli are located in /src/test/resources

### James
- Integration of [FFT](https://github.com/ucb-art/fft/tree/diplomacyPort2) block with lab2 (cordic) template repo.
- Attaching FFT and FFTBlock wrapper to a ReadQueue and WriteQueue that have been adapted for multiple FFT lanes.
- ReadQueue and WriteQueue contain a queue for each lane of the FFT.
- Modules are connected Diplomatically with StreamNodes.
![alt text](https://github.com/ucberkeley-ee290c/fa18-mimo/blob/master/doc/tapein0_james.png "FFT tapein 0 diagram")

### Victor
-

### Yue
- 

