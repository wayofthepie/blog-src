---
title: Installing NixOs
tags: nix, nixos, systems
---
# Introduction
I've recently started diving into _[Nix](https://nixos.org/nix/)_ and 
_[NixOS](https://nixos.org/)_. __Nix__ is a package manager for Linux/Unix systems, a
quick description from its own site:

> Nix's purely functional approach ensures that installing or upgrading one
> package cannot break other packages. This is because it won't overwrite
> dependencies with newer versions that might cause breakage elsewhere. It
> allows you to roll back to previous versions, and ensures that no package is
> in an inconsistent state during an upgrade.

__NixOs__ is an operating system which takes a declarative
approach to configuration and package management and uses Nix as its package
manager. Following is an example of creating a user on a NixOS system:

```haskell
...
users.extraUsers.alice =
  { isNormalUser = true;
    home = "/home/alice";
    extraGroups = [ "wheel" ];
  };
security.sudo.enable = true;
...
```
An example of a fully configured NixOS system can be found
at <https://github.com/wayofthepie/sky>, this is the configuration I'm currently
using for my desktop.

In this post I'm going to run through how I installed NixOs, with the aim of helping anyone new
to NixOs through the installation process. Throughout the post I will be assuming
the following:

* The NixOS install CD or USB installer - see
 <https://nixos.org/nixos/manual/index.html#sec-installation>.
* An empty disk.
* Boot with UEFI.

These are not requirements, but will make the post easier to follow.

# Partition
Let's start! First boot into NixOs, whether from USB or the CD installer,
and find the disk you wish to install onto. 
I  will use __/dev/sda__ in this post, you can replace this 
with whatever disk you are going to use.   

The system will boot with UEFI so __gdisk__ should be used to create the 
partitions. This will create a GPT (GUID Partition Table) formatted disk.

With __gdisk__ create the following partitions:

| Partition | Size | Type               | Code | Filesystem|
|-----------|------|--------------------|------|-----------|
| /dev/sda1 | 2 MiB | _Bios Boot Partition_ |ef02 |None | 
| /dev/sda2 | 1024 MiB | *EFI* | ef00 | vfat | 
| /dev/sda3 | 119 GiB | _Linux LVM_  | 8e00 | xfs |

If you have never used __gdisk__ before, fear not! Just run `gdisk /dev/sda`{.bash}
and answer the questions as outlined in the next section. If you know what you are 
doing, you can skip the next section.

## Creating Partitions With gdisk
```bash
$ gdisk /dev/sda

GPT fdisk (gdisk) version 1.0.1

Partition table scan:
  MBR: not present
  BSD: not present
  APM: not present
  GPT: not present

Creating new GPT entries.

Command (? for help): n
Partition number (1-128, default 1): 1
First sector (34-251658206, default = 2048) or {+-}size{KMGTP}: 
Last sector (2048-251658206, default = 251658206) or {+-}size{KMGTP}: +2M
Current type is 'Linux filesystem'
Hex code or GUID (L to show codes, Enter = 8300): ef02
Changed type of partition to 'BIOS boot partition'

Command (? for help): n
Partition number (2-128, default 2): 
First sector (34-251658206, default = 6144) or {+-}size{KMGTP}: 
Last sector (6144-251658206, default = 251658206) or {+-}size{KMGTP}: +1G
Current type is 'Linux filesystem'
Hex code or GUID (L to show codes, Enter = 8300): ef00
Changed type of partition to 'EFI System'

Command (? for help): n
Partition number (3-128, default 3): 
First sector (34-251658206, default = 2103296) or {+-}size{KMGTP}: 
Last sector (2103296-251658206, default = 251658206) or {+-}size{KMGTP}: 
Current type is 'Linux filesystem'
Hex code or GUID (L to show codes, Enter = 8300): 8e00
Changed type of partition to 'Linux LVM'

Command (? for help): w

Final checks complete. About to write GPT data. THIS WILL OVERWRITE EXISTING
PARTITIONS!!

Do you want to proceed? (Y/N): y
OK; writing new GUID partition table (GPT) to /dev/sda.
The operation has completed successfully.
```

In the above, we begin creating our first  new partition by using the command __n__ when asked the
question `Command (? for help):`{.bash}. 

The first question we are asked is what number we would like to give, it's our 
first partition so __1__, this will create a partition called __/dev/sda1__ 
on __/dev/sda__. 

Next, we are asked what sector to start on and what sector to end on. The starting
sector can be left empty - this means start at next unused sector, which in this
case is the start of the disk. As for which sector to end on, here we answer __+2M__ 
which will means _move 2 Mebibytes [^1] (__MiB__) forward from the starting sector_, 
giving us a partition size of 2 Mebibytes. 

Finally, we are asked if we want to change the type of the partition. Our first
partition should have the type __ef02__, more on why later.

That's the first partition created, it should be clear now how the next two are
created.

## Notes On The Partitions
Done? Good, to get a look at the disk layout run `gdisk -l /dev/sda`{.bash},
this should print out the following:

```bash
$ gdisk -l /dev/sda

GPT fdisk (gdisk) version 1.0.1

Partition table scan:
  MBR: protective
  BSD: not present
  APM: not present
  GPT: present

Found valid GPT with protective MBR; using GPT.
Disk /dev/sda: 251658240 sectors, 120.0 GiB
Logical sector size: 512 bytes
Disk identifier (GUID): 031652EE-C219-4EFB-84AB-9E310B14357E
Partition table holds up to 128 entries
First usable sector is 34, last usable sector is 251658206
Partitions will be aligned on 2048-sector boundaries
Total free space is 2014 sectors (1007.0 KiB)

Number  Start (sector)    End (sector)  Size       Code  Name
   1            2048            6143   2.0 MiB     EF02  BIOS boot partition
   2            6144         2103295   1024.0 MiB  EF00  EFI System
   3         2103296       251658206   119.0 GiB   8E00  Linux LVM
```

### Grub Compatibility: /dev/sda1
This is used by __grub__ [^2] at boot-time when booting off a GPT disk. 

### EFI Partition: /dev/sda2
UEFI will load files stored in this partition when booting.

### Main Data Partition: /dev/sda3
This is used for creating the logical volumes __swap__ (4GB), __/__ (20GB), 
__/home__ (20GB) and __/opt__ (20GB) in this post, it's size should reflect
the sum of the sizes of all logical volumes you wish to create. Note that
any free space left of the disk can be used later, and those logical
volumes can be increased (or decreased) in size.


# Linux Unified Key Setup (LUKS)
Now that the partitions are setup, it's time to encrypt the main partition,
__/dev/sda3__. This is optional and there are a few ways to do it, this post uses
_[LUKS](https://guardianproject.info/code/luks/)_.

The _[cryptsetup](https://gitlab.com/cryptsetup/cryptsetup)_ tool is used to
create __LUKS__ volumes. To search nix packages the command `nix-env -qaP packageName`{.bash} [^3] 
can be used:

```bash
$ nix-env -qaP cryptsetup

nixos.cryptsetup  cryptsetup-1.7.0
```

Now that we know __cryptsetup__ is in the package list, we can install it with
`nix-env`{.bash}:

```bash
$ nix-env -i cryptsetup

installing 'cryptsetup-1.7.0'
building path(s) '/nix/store/86mqcs95fb3gkkb74481fbf90zh5w98l-user-environment'
created 44 symlinks in user environment
```

## Encrypting /dev/sda3
I won't go into detail about __LUKS__ or __cryptsetup__ here, we will also just
use the defaults for both. Simply run the following __cryptsetup__ command and
enter a passphrase:

```bash
$ cryptsetup -y -v luksFormat /dev/sda3

WARNING!
========
This will overwrite data on /dev/sda3 irrevocably.

Are you sure? (Type uppercase yes): YES
Enter passphrase: 
Verify passphrase: 
Command successful.

```

__/dev/sda3__ is now encrypted! [^4] The options passed to _cryptsetup_ have 
the following meaning:

* __-y__ : prompt for passphrase twice.
* __-v__ : verbose mode.
* __luksFormat__ : initializes  a  LUKS partition and sets the initial
  passphrase (for key-slot 0) [^5].

Great, we now have an encrypted disk, but how is it used? Let's jump back to
__cryptsetup__, it has a command  `cryptsetup luksOpen LUKS_DEVICE NAME`{.bash} 
which, when it verifies the passphrase (in our case), will open the __LUKS__ 
device and create a mapping to it (on the path __/dev/mapper/NAME__)from the given name.

```bash
$ cryptsetup luksOpen /dev/sda3 enc-data

Enter passphrase for /dev/sda3: 

$ ls -la /dev/mapper/
total 0
drwxr-xr-x  2 root root      80 Jul  9 18:43 .
drwxr-xr-x 18 root root    3320 Jul  9 18:43 ..
lrwxrwxrwx  1 root root       7 Jul  9 18:43 enc-data -> ../dm-0

```

Sweet! We now have an encrypted partition and know how to access it. All we have
left to do is setup logical volumes on the partition.

# LVM Setup
Make sure our data partition - __/dev/sda3__ - is open and mapped to the path
__/dev/mapper/enc-data__ with __luksOpen__, as mentioned above. 

There are three steps to setting up _logical volumes_, creating a _physical volume_
(PV), a _volume group_ (VG) and a _logical volume_ (LV). If you are unfamiliar with 
Logical Volume Management (LVM) the [Arch Linux Wiki
entry](https://wiki.archlinux.org/index.php/LVM) is a good resource.

As a quick description, a physical disk can be divided into one or more
physical volumes, a volume group is made up of one or mode physical volumes and
logical volumes are created within volume groups.

## Physical Volume
`pvcreate`{.bash} is the command used to create physical volumes. Let's use this
to create a physical volume on __/dev/mapper/enc-data__:

```bash
$ pvcreate /dev/mapper/enc-data 

Physical volume "/dev/mapper/enc-data" successfully created
```

`pvdisplay`{.bash} is used to display information on physical volumes on the 
system.

```bash
$ pvdisplay

"/dev/mapper/enc-data" is a new physical volume of "119.00 GiB"
--- NEW Physical volume ---
PV Name               /dev/mapper/enc-data
VG Name               
PV Size               119.00 GiB
Allocatable           NO
PE Size               0   
Total PE              0
Free PE               0
Allocated PE          0
PV UUID               H3bhmD-KR9n-XaHb-03L6-TpN2-pYB5-DtUG7x
```

## Volume Group
`vgcreate`{.bash} is used to create a volume group. Lets create a volume group
called __vg__.

```bash
$ vgcreate vg /dev/mapper/enc-data

Volume group "vg" successfully created

```

`vgdisplay`{.bash} displays info on the systems volume groups.

```bash
$ vgdisplay

--- Volume group ---
VG Name               vg
System ID             
Format                lvm2
Metadata Areas        1
Metadata Sequence No  1
VG Access             read/write
VG Status             resizable
MAX LV                0
Cur LV                0
Open LV               0
Max PV                0
Cur PV                1
Act PV                1
VG Size               118.99 GiB
PE Size               4.00 MiB
Total PE              30462
Alloc PE / Size       0 / 0   
Free  PE / Size       30462 / 118.99 GiB
VG UUID               O9EIaS-kX9Y-EXjo-I3zU-mPXY-9SVB-br2fEy
```

## Logical Volume
`lvcreate`{.bash} is used to create a logical volume. In this case, we want to
create four logical volumes from our volume group __vg__.

```bash
lvcreate -n swap --size 8G vg
lvcreate -n root --size 20G vg
lvcreate -n home --size 20G vg
lvcreate -n opt --size 20G vg
```

Running the above four lvcreate commands will give us our logical volumes __swap__, 
__root__, __home__ and __opt__. Use `lvdisplay`{.bash} to get more info on 
them:

```bash
$ lvdisplay

--- Logical volume ---
LV Path                /dev/vg/swap
LV Name                swap
VG Name                vg
LV UUID                F5DbgU-ymix-24Bh-JMTy-q2kL-ayvN-UpNUFB
LV Write Access        read/write
LV Creation host, time nixos, 2016-07-09 19:11:46 +0000
LV Status              available
# open                 0
LV Size                8.00 GiB
Current LE             2048
Segments               1
Allocation             inherit
Read ahead sectors     auto
- currently set to     256
Block device           254:1
 
--- Logical volume ---
LV Path                /dev/vg/root
LV Name                root
VG Name                vg
LV UUID                FNLEbI-z05h-J7br-OMst-Ui0t-yq3r-BMNVjA
LV Write Access        read/write
LV Creation host, time nixos, 2016-07-09 19:11:46 +0000
LV Status              available
# open                 0
LV Size                20.00 GiB
Current LE             5120
Segments               1
Allocation             inherit
Read ahead sectors     auto
- currently set to     256
Block device           254:2
 
--- Logical volume ---
LV Path                /dev/vg/home
LV Name                home
VG Name                vg
LV UUID                zBLqKD-S6n7-BcTh-1oAF-TLvV-26Oc-dQ3wQR
LV Write Access        read/write
LV Creation host, time nixos, 2016-07-09 19:11:46 +0000
LV Status              available
# open                 0
LV Size                20.00 GiB
Current LE             5120
Segments               1
Allocation             inherit
Read ahead sectors     auto
- currently set to     256
Block device           254:3
 
--- Logical volume ---
LV Path                /dev/vg/opt
LV Name                opt
VG Name                vg
LV UUID                hr3GRw-d0P4-hDIw-HRIo-hqKK-Lql9-gD58a7
LV Write Access        read/write
LV Creation host, time nixos, 2016-07-09 19:11:47 +0000
LV Status              available
# open                 0
LV Size                20.00 GiB
Current LE             5120
Segments               1
Allocation             inherit
Read ahead sectors     auto
- currently set to     256
Block device           254:4
```

Ok! That's our disk setup ~90% complete.

#Final Disk Setup
All we have left to do now is format the boot partition and our logical volumes 
and we ca start preparing NixOs for install. The formatting can be done with a short
script:

```bash
mkfs.vfat -n BOOT /dev/sda2
mkswap -L swap /dev/vg/swap
for lv in root home opt; do 
  mkfs.xfs -L $lv /dev/vg/$lv
done
```

This formats __/dev/sda2__ with vfat and labels it __BOOT__, sets up a swap area
on our LV  __/dev/vg/swap__ and finally formats our LV's __/dev/vg/root__, __/dev/vg/home__
and __/dev/vg/opt__ with __xfs__.

Awesome! Disk setup is now 100% complete! 

# Install NixOS
Now that our disk is ready we can prepare to install NixOs, this is pretty
short, and sweet. 

First, we need to create the directories __/mnt/boot__, __/mnt/home__ and 
__/mnt/opt__. Then, we need to mount our LV's - that we created in the previous
section - and our boot partition:

```bash
mkdir /mnt/{boot,home,opt}
mount /dev/vg/root /mnt
mount /dev/vg/home /mnt/home
mount /dev/vg/opt /mnt/opt
mount /dev/sda2 /mnt/boot
```

Before we install NixOs, we have to run `nixos-generate-config 
--root /mnt/`{.bash} [^6]:

```bash
$ nixos-generate-config --root /mnt/

writing /mnt/etc/nixos/hardware-configuration.nix...
writing /mnt/etc/nixos/configuration.nix...

```
* **/mnt/etc/nixos/hardware-configuration.nix** contains NixOs configuration options based
on current hardware config.
* **/mnt/etc/nixos/configuration.nix** is the main NixOs system configuration
  module.

We need to add some extra information to **/mnt/etc/nixos/configuration.nix** to
tell NixOs what our boot device is, and the fact that it is also a LUKS device:

```haskell
boot.loader.grub.device = "/dev/sda";

boot.initrd.luks.devices = [{
  name = "root";
  device = "/dev/sda3";
  preLVM = true;
}];

```
This can be added anywhere in the file, however there should already be a
section for __boot.loader.grub.device__ which is commented out. If so, uncomment
it and add the config for LUKS (__boot.initrd.luks.devices__) right after it.

Now we can install! Just run:

```bash
nixos-install
```

This can take some time, you should see information on what packages are being
downloaded and installed while the installation is running. You will be prompted
to enter the __root__ password during the installationi, once you do that the
installation has completed and you can boot up your new NixOs install! [^7] 

# Conclusion
The aim of this post was just to get NixOs installed, using the steps I used
on my own system. I plan on doing some more posts on Nix and NixOs over the
coming weeks, with the overall goal being to gain a better understanding of
both.

Once I have that, I'll start diving into more interesting topics like NixOps and
the use of the three in Configuration Management across multiple connected
systems.

Next post will be on the use of Nix in setting up a development environment
in multiple languages, stay tuned!

[^1]: 1 MiB = 2<sup>20</sup> bytes = 1024 kibibytes = 1048576 bytes.
[^2]: See [Arch Linux - Grub GPT](
https://wiki.archlinux.org/index.php/GRUB#GUID_Partition_Table_.28GPT.29_specific_instructions)
[^3]: Short for `nix-env --query --available --attr-path packageName`{.bash}. This searches
all available packages for the packageName, and will also print out its
attribute path (see [Install By Attribute
Path](https://nixos.org/wiki/Howto_find_a_package_in_NixOS#Install_by_attribute)).
[^4]: Running `cryptsetup -y -v luksFormat /dev/sda3`{.bash} will use the
default LUKS settings, which can be found by running `cryptsetup --help`{.bash}.
In my case (v1.7.0 of __cryptsetup__) they are _LUKS1: aes-xts-plain64, Key: 256 bits, LUKS header hashing:
sha256, RNG: /dev/urandom_.
[^5]: See
[Arch Linux - Luks KeyMangement](https://wiki.archlinux.org/index.php/Dm-crypt/Device_encryption#Key_management)
for more information.
[^6]: For more info on `nixos-generate-config`{.bash} see Section 10 of the [Nix
Install Manual](https://nixos.org/nixos/manual/#sec-installation).
[^7]: When the system boots you will be asked for a password _before_ the login prompt, 
this is the password you used to create the LUKS volume.
 