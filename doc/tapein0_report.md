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
- Python system model for each blocks are created and tested using the SDK 
- The system contains:
    - packet_gen: transmit signals for each user
        ![trans sig](https://github.com/ucberkeley-ee290c/fa18-mimo/blob/master/doc/tx_sig.png)
        - Pilot and payload generator
        - Root-Raised Cosine filter
    - channel_gen: generated the channel for simulation
        - FlatIID: frequecy flat channel (random phase shifts for different users to different antennas)
        - Rician: frequecy selective channel
    - rx_filter: a root-raised cosine filter at rx, with oversampling incoming signals
    - golay_corr: Golay channel estimator
        - For coarse timing
        - Channel estimator assuming channel is frequency flat
    - BF_panel: Matric multiplication between filtered signals with the Hermit conjegate estimated channel matrix in time domain
        - Gain power for different users
    - downsample: downsample signals based on the RX oversampled ratio
    - sig_fft: FFT for all symbol and the size is the number of subcarriers (CP needs to be abandoned)
    - FreqChannelEst: Estimating channel for each subcarriers at each antenna using OFDM pilots from different users
        - The gain found by antenna m and user k can set the H[m,k] coefficient for different subcarriers
    - FreqBF: Matric multiplication between filtered signals with the Hermit conjegate estimated channel matrix in frequency domain for different subcarriers
    - ZFMatrix: matrix inverting



