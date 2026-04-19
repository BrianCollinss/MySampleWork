# APSIM Next Generation

Source URL: https://apsimnextgeneration.netlify.app/usage/commandline/command-line-linux
Source domain: apsimnextgeneration.netlify.app
Fetched at: 2026-04-19T09:51:24.526274+00:00

The next generation of APSIM
>
Usage
>
Command Line
> Command Line on Linux
Command Line on Linux
Install
There are two options, option 1 is recommended for users running APSIM on cloud infrastructure (HPC, AWS, Azure) or WSL (Windows Subsystem for Linux)
run APSIM using a docker container
install APSIM using debian file (.deb) to see how to do this click
here
Running APSIM on Linux using Docker container
Note: it helps to be familiar with the linux command line basics to perform the following steps.
Make sure you have the docker engine installed. Instructions on how to do this can be found
here
Pull down the apsimng docker image using:
docker pull apsiminitiative/apsimng
To see an example of how to run the wheat example, follow the instructions from the
APSIM.Docker GitHub repo
otherwise continue.
Organise your files into a directory. I’ll use an example directory here called
test-run
Place an Apsimx file in this directory called
Wheat.apsimx
Change directory to the newly created test-run directory.
Run the simulation using this command:
docker run -i --rm -v "$PWD:/test-run" apsiminitiative/apsimng /test-run/Wheat.apsimx
You can use any commands available for the APSIM command line tool such as
--apply
or
--csv
after the
apsiminitiative/apsimng
command portion in the above docker command. The above command simply ran the Wheat.apsimx file we put in the test-run directory.
