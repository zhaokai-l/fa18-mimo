## Tape-In 0

The following items are in tape-in 0:
- UE and channel model in Python, borrowing as much as possible from the Hydra discrete codebase.
- The UE will switch between transmitting a time-domain BPSK pilot/payload and OFDM. If in time-domain, there will be an IFFT
- The channel model will convolve and quantize.
- The samples will be outputted as a file, and read in via a Rocket C program to a memory-mapped IO.
Base station will consist of two paths:
- Time domain-single carrier, using a Golay correlator and single MRC matrix-multiply
- Frequency-domain FFT-bsaed beamforming without Golay correlator (since Golay correlator is inherently time-domain), with multiple MRC matrix multiplies.
- Tape-in 0 will have 1 user, so matrices are size Mx1.
- The FSM is responsible for peak detection and outputting mag/phase of peaks to the elements of the beam-forming matrix. In time-domain path, this will look at Golay channel impulse response; in FFT path, it will look at the OFDM bins.
Rocket will be attached and use to set the following registers:
- Mux selects for OFDM vs freq. flat paths.
- Guard interval for Golay corerelator.
- Weights override for matrix multiply.

![Tape-in 0 Block Diagram](https://github.com/ucberkeley-ee290c/fa18-mimo/blob/master/doc/tapein0.png)