# Variables
DOCKER_IMG := ghcr.io/open-aims/ltmp_dashboard:dev
TAR        := ltmp_dashboard.tar
SIF        := ltmp_dashboard.sif
SANDBOX    := ltmp_dashboard_sandbox
UPDATED    := ltmp_dashboard_updated.sif
BIN_FILE   := /usr/local/lib/R/site-library/INLA/bin/linux/64bit/inla.mkl.run

build_docker:
  @echo "Building Docker image..."
  # docker build --tag ghcr.io/open-aims/ltmp_dashboard:dev .
  docker build --tag $(DOCKER_IMG) .

pull_docker:
  @echo "Pulling Docker image..."
  # docker pull ghcr.io/open-aims/ltmp_dashboard:dev 
  docker pull $(DOCKER_IMG)

build_singularity:
   @echo "Building singularity image..."
   # docker save ghcr.io/open-aims/ltmp_dashboard:dev -o ltmp_dashboard.tar
   docker save $(DOCKER_IMG) -o $(TAR)
   # apptainer build ltmp_dashboard.sif docker-archive://ltmp_dashboard.tar
   apptainer build $(SIF) docker-archive://$(TAR)
   ## Now have to create a sandbox version
   # apptainer build --sandbox ltmp_dashboard_sandbox.sif ltmp_dashboard.sif
   apptainer build --sandbox $(SANDBOX) $(SIF)
   ## enter sandbox with writable permissions
   # sudo apptainer shell --writable ltmp_dashboard_sandbox.sif
   # chmod +x /usr/local/lib/R/site-library/INLA/bin/linux/64bit/inla.mkl.run
   # exit
   sudo apptainer exec --writable $(SANDBOX) chmod +x $(BIN_FILE)
   ## rebuild image
   apptainer build $(UPDATED) $(SANDBOX)
