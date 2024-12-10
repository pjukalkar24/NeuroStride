# NeuroStride
<strong>A smart knee brace to prevent falls in individuals with Multiple Sclerosis.</strong>


## Running this Repository

sensor_alignment.py --> works as a logger to save alignment movement data for later use (saves to sensor_alignment.csv)

sensor_data.py --> works as a logger for general purposes (saves to sense_log.csv)

average_detection.py --> uses data produced by sensor_alignment.py to find the average values used in stride detection (saves to average_value.txt)

stride_detection.py --> decides when to fire device through average value method and vector method

order of execution: sensor_alignment.py, average_detection.py, stride_detection.py
