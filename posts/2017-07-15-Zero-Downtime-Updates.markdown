---
title: "Terraform, Google Cloud And Kubernetes"
tags: systems,infra-auto
---

# Introduction
I've been hacking about with automated infrastructure setup a lot lately. The two tools
I've focused on the most are NixOps and Terraform. This post is about the use of terraform
on [Google Cloud Platform](https://cloud.google.com/docs/) (GCP) to create and manage a
Kubernetes Container Cluster.

## Setup
Before we begin, if you want to run any of the code, you'll need an account on google cloud.
If you do not know what either
[GCP](https://www.terraform.io/docs/providers/google/index.html#authentication-json-file),
[Terraform](https://www.terraform.io/docs/providers/google/index.html#authentication-json-file)
or [Kubernetes](https://www.terraform.io/docs/providers/google/index.html#authentication-json-file)
are you should follow those links. Note that you will also need the
[google cloud sdk](https://cloud.google.com/sdk/) (as we will be using the `gcloud` cli) and also
the kubernetes cli, [kubectl](https://kubernetes.io/docs/tasks/tools/install-kubectl/).
Once you are ready, you will need to create an auth json file as described
[here](https://www.terraform.io/docs/providers/google/index.html#authentication-json-file), and
that should be everything you need to proceed.

# Terraform Google Cloud Provider
Terraform's [Google Cloud provider](https://www.terraform.io/docs/providers/google/index.html) covers a lot
of the functionality of GCP. It also has a [backend](https://www.terraform.io/docs/backends/index.html)
for storing state on [Google Cloud Storage](https://cloud.google.com/storage/) (GCS). By default
terraform will store the state locally, so this backend is not required, but it is good practice [^1].

Let's set up our backend to store terraforms state on GCS. In a file called `backend.tf`:

```
terraform {
  backend "gcs" {
    bucket  = "my-bucket"
    path    = "my-folder/cluster-infra/terraform.tfstate"
    project = "my-project"
  }
}
```
This tells terraform it should store and lookup state in GCS in the project `my-project`,
a bucket within that project, `my-bucket`, and the path `my-folder/cluster-infra/terraform.tfstate`
within that bucket. You need to make sure this project, bucket and path exist.
Just a few more lines, and we can build our cluster!

## Cluster Definition
The definition for the [Container Cluster](https://cloud.google.com/container-engine/)
itself is quite short. Let's create another file called `init.tf`.

```
provider "google" {
  region  = "${var.region}"
  project = "${var.project}"
}

resource "google_container_cluster" "primary" {
  name = "test-cluster"
  zone = "${var.zone}"
  initial_node_count = 3

  master_auth {
    username = "${var.kube_username}"
    password = "${var.kube_password}"
  }

  node_config {
    oauth_scopes = [
      "https://www.googleapis.com/auth/compute",
      "https://www.googleapis.com/auth/devstorage.read_only",
      "https://www.googleapis.com/auth/logging.write",
      "https://www.googleapis.com/auth/monitoring",
    ]
  }
}

```
That's it! This will build a three node kubernetes container cluster. Breaking it down:

  * `provider "google"` : this says we want to use the
    [Google Cloud Provider](https://www.terraform.io/docs/providers/google/index.html).
      * `region` : the [region](https://cloud.google.com/compute/docs/regions-zones/regions-zones)
        to spin up our resources.
      * `project` : the project our resources should live within.
  * `resource "google_container_cluster" "primary"` [^2]: create a
    [google_container_cluster](https://www.terraform.io/docs/providers/google/r/container_cluster.html)
    that can be referenced from other terraform resources but the name `primary`.
      * `name` : the name of this cluster _within google cloud_.
      * `zone` : the [zone](https://cloud.google.com/compute/docs/regions-zones/regions-zones)
        in the region we specified in our provider to spin up these resources.
      * `initial_node_count` : the number of nodes in this cluster.
      * `master_auth` : the credentials we can use to access the kubernetes cluster
      * `node_config` : the machine type and image used for all nodes, here we just define
        [oauth_scopes](https://www.terraform.io/docs/providers/google/r/container_cluster.html#oauth_scopes).

## Variables
In the above configs you probably noticed `${var.something}` in a few places. Values for these variables
can be loaded into the config when launching terraform in multiple ways [^3]. For this post I'll go the
variable files route. First create a `variables.tf` with the following definitions:

```
variable "project" {}
variable "region" {}
variable "zone" {}
variable "kube_username" {}
variable "kube_password" {}
```

Now, create a file `terraform.tfvars` with the following key/value pairs:

```
project = "my-project"
region = "europe-west1"
zone = "europe-west1-b"
kube_username = "testuser"
kube_password = "testpassword"
```

# Launching Our Cluster
Let's put everything together and launch our cluster! You should have the following four files:

```
$ ls
backend.tf  init.tf  terraform.tfvars  variables.tf
```

There is one more step before we can launch. If you try to run `terraform apply`
or `terraform plan` you will get the following error:

```{.bash}
$ terraform plan
Backend reinitialization required. Please run "terraform init".
Reason: Initial configuration of the requested backend "gcs"

The "backend" is the interface that Terraform uses to store state,
perform operations, etc. If this message is showing up, it means that the
Terraform configuration you're using is using a custom configuration for
the Terraform backend.

...
```
Trying to run `terraform init` will also give an error:

```{.bash}
$ terraform init
Initializing the backend...

Error configuring the backend "gcs": Failed to configure remote backend "gcs": google: could not find default credentials.
See https://developers.google.com/accounts/docs/application-default-credentials for more information.

Please update the configuration in your Terraform files to fix this error
then run this command again.
```

What we need to do to correctly initialize the backend is pass the json credentials file created
in the **Setup** section above. If you missed this, the instructions can be found
[here](https://www.terraform.io/docs/providers/google/index.html#authentication-json-file).
Download the json file and place it under `~/.gcp_creds.json`. Now we can finally start running things!

The envvar `GOOGLE_APPLICATION_CREDENTIALS` tells terraform where to find the creds. To initialize the backend:
```{.bash}
$ GOOGLE_APPLICATION_CREDENTIALS=~/.gcp_creds.json terraform init
Initializing the backend...


Successfully configured the backend "gcs"! Terraform will automatically
use this backend unless the backend configuration changes.

Terraform has been successfully initialized!
...
```

Let's see what terraform will build with `terraform plan`:
```{.bash}
$ GOOGLE_APPLICATION_CREDENTIALS=~/.gcp_creds.json terraform plan
Refreshing Terraform state in-memory prior to plan...
The refreshed state will be used to calculate this plan, but will not be
persisted to local or remote state storage.

The Terraform execution plan has been generated and is shown below.
Resources are shown in alphabetical order for quick scanning. Green resources
will be created (or destroyed and then created if an existing resource
exists), yellow resources are being changed in-place, and red resources
will be destroyed. Cyan entries are data sources to be read.

Note: You didn't specify an "-out" parameter to save this plan, so when
"apply" is called, Terraform can't guarantee this is what will execute.

+ google_container_cluster.primary
    additional_zones.#:                   "<computed>"
    cluster_ipv4_cidr:                    "<computed>"
    endpoint:                             "<computed>"
    initial_node_count:                   "3"
    instance_group_urls.#:                "<computed>"
    logging_service:                      "<computed>"
    master_auth.#:                        "1"
    master_auth.0.client_certificate:     "<computed>"
    master_auth.0.client_key:             "<sensitive>"
    master_auth.0.cluster_ca_certificate: "<computed>"
    master_auth.0.password:               "<sensitive>"
    master_auth.0.username:               "kubeuser"
    monitoring_service:                   "<computed>"
    name:                                 "vulgr-cluster"
    network:                              "default"
    node_config.#:                        "1"
    node_config.0.disk_size_gb:           "<computed>"
    node_config.0.image_type:             "<computed>"
    node_config.0.local_ssd_count:        "<computed>"
    node_config.0.machine_type:           "<computed>"
    node_config.0.oauth_scopes.#:         "4"
    node_config.0.oauth_scopes.0:         "https://www.googleapis.com/auth/compute"
    node_config.0.oauth_scopes.1:         "https://www.googleapis.com/auth/devstorage.read_only"
    node_config.0.oauth_scopes.2:         "https://www.googleapis.com/auth/logging.write"
    node_config.0.oauth_scopes.3:         "https://www.googleapis.com/auth/monitoring"
    node_config.0.service_account:        "<computed>"
    node_pool.#:                          "<computed>"
    node_version:                         "<computed>"
    zone:                                 "europe-west1-b"


Plan: 1 to add, 0 to change, 0 to destroy.
```
Finally, let's build our cluster:

```{.bash}
$ GOOGLE_APPLICATION_CREDENTIALS=~/.gcp_creds.json terraform apply
google_container_cluster.primary: Creating...
  additional_zones.#:                   "" => "<computed>"
  cluster_ipv4_cidr:                    "" => "<computed>"
  endpoint:                             "" => "<computed>"
  initial_node_count:                   "" => "3"
  instance_group_urls.#:                "" => "<computed>"
  logging_service:                      "" => "<computed>"
  master_auth.#:                        "" => "1"
  master_auth.0.client_certificate:     "" => "<computed>"
  master_auth.0.client_key:             "<sensitive>" => "<sensitive>"
  master_auth.0.cluster_ca_certificate: "" => "<computed>"
  master_auth.0.password:               "<sensitive>" => "<sensitive>"
  master_auth.0.username:               "" => "kubeuser"
  monitoring_service:                   "" => "<computed>"
  name:                                 "" => "vulgr-cluster"
  network:                              "" => "default"
  node_config.#:                        "" => "1"
  node_config.0.disk_size_gb:           "" => "<computed>"
  node_config.0.image_type:             "" => "<computed>"
  node_config.0.local_ssd_count:        "" => "<computed>"
  node_config.0.machine_type:           "" => "<computed>"
  node_config.0.oauth_scopes.#:         "" => "4"
  node_config.0.oauth_scopes.0:         "" => "https://www.googleapis.com/auth/compute"
  node_config.0.oauth_scopes.1:         "" => "https://www.googleapis.com/auth/devstorage.read_only"
  node_config.0.oauth_scopes.2:         "" => "https://www.googleapis.com/auth/logging.write"
  node_config.0.oauth_scopes.3:         "" => "https://www.googleapis.com/auth/monitoring"
  node_config.0.service_account:        "" => "<computed>"
  node_pool.#:                          "" => "<computed>"
  node_version:                         "" => "<computed>"
  zone:                                 "" => "europe-west1-b"
google_container_cluster.primary: Still creating... (10s elapsed)
...
google_container_cluster.primary: Still creating... (3m0s elapsed)
google_container_cluster.primary: Creation complete (ID: test-cluster)

Apply complete! Resources: 1 added, 0 changed, 0 destroyed.

The state of your infrastructure has been saved to the path
below. This state is required to modify and destroy your
infrastructure, so keep it safe. To inspect the complete state
use the `terraform show` command.

State path:
...
```

## List The Nodes
Did it actually work? Let's test. To retrieve the credentials and load the context for
our cluster into kubectl:

```{.bash}
$ gcloud container clusters get-credentials test-cluster --zone ${our_zone} --project my-project
Fetching cluster endpoint and auth data.
kubeconfig entry generated for test-cluster.
```

Switch to this context in kubectl:

```{.bash}
$ kubectl config set-cluster test-cluster
Cluster "test-cluster" set.
```

Now should be able to list the nodes:

```{.bash}
$ kubectl get nodes
NAME                                          STATUS    AGE
gke-test-cluster-default-pool-a1844955-h5w0   Ready     3m
gke-test-cluster-default-pool-a1844955-vc3l   Ready     3m
gke-test-cluster-default-pool-a1844955-wf4v   Ready     3m
```

Excellent! To destroy the cluster simply run:

```{.bash}
$ GOOGLE_APPLICATION_CREDENTIALS=~/.gcp_creds.json terraform destroy
Do you really want to destroy?
  Terraform will delete all your managed infrastructure.
  There is no undo. Only 'yes' will be accepted to confirm.

  Enter a value: yes

google_container_cluster.primary: Refreshing state... (ID: test-cluster)

Destroy complete! Resources: 0 destroyed.
```


# Conclusion
There's not much to codifying a cluster setup on Google Cloud. Note that there are some limitations,
one of the bigger ones being updates, you cannot update the `google_container_cluster` without terraform
destroying the initial cluster and createing a new one. Depending on how you plan to apply updates this may
or may not be a problem - for example you could choose to create _an entire new cluster_ with updates
and migrate any existing workloads on the old onto the new , finally destroying the old one.

Now that you have a kubernetes cluster, you can also manage this using the
[Kubernetes Provider](https://www.terraform.io/docs/providers/kubernetes/index.html), I'll leave that
for another post.

The code from this post was adapted from a project I'm toying about with. You can view the code up
to this post [here](https://github.com/wayofthepie/forge-gcp/tree/tfblog01),
note that some resource values are different.

[^1]: [Remote State](https://www.terraform.io/docs/state/remote.html).
[^2]: [google_container_cluster](https://www.terraform.io/docs/providers/google/r/container_cluster.html).
[^3]: See the overview at [variables](https://www.terraform.io/docs/configuration/variables.html).
