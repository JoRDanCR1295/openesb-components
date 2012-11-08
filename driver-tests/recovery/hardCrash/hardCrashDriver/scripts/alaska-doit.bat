goto start
:start
@rem ------------------------------------------------------------------------------------------------------------
@rem 0. Start with an clean directory
@rem ------------------------------------------------------------------------------------------------------------
cd ${SRCROOT}/Sun\appserver\bin
call asadmin stop-domain domain1
cd ${SRCROOT}
rmdir /S /Q cache
rmdir /S /Q capstool
rmdir /S /Q jbicomps
rmdir /S /Q nb55
rmdir /S /Q nb_all
rmdir /S /Q root
rmdir /S /Q Sun

@rem ------------------------------------------------------------------------------------------------------------
@rem 1. Installing Sun Appserver 9
@rem ------------------------------------------------------------------------------------------------------------
mkdir Sun
xcopy /E \\tahoe\downloads\Sun Sun
cd ${SRCROOT}/Sun\appserver\bin
call asadmin start-domain domain1

@rem ------------------------------------------------------------------------------------------------------------
@rem 2. Checkout Alaska
@rem ------------------------------------------------------------------------------------------------------------
cd ${SRCROOT}
set CVSUSER=kbhumana
set CVSROOT=:pserver:%CVSUSER%@cvsserver.stc.com:/cvs/Alaska
cvs co jbicomps capstool root/env.bat root/build.xml root/sharedlibrary root/jbi/sharedlibrary root/build-tools
cd ${alaska_root}
call env.bat
@rem pause

@rem ------------------------------------------------------------------------------------------------------------
@rem 3. Building Enterprise Pack from sources on top of NetBeans IDE binary
@rem ------------------------------------------------------------------------------------------------------------
cd ${SRCROOT}
mkdir nb55
xcopy /E \\tahoe\downloads\nb55 nb55
cd nb55
call nb55-enterprise-pack-doit.bat

@rem ------------------------------------------------------------------------------------------------------------
@rem 4. Building and Installing Alaska designtime, runtime components
@rem ------------------------------------------------------------------------------------------------------------
@rem Designtime
@rem ------------------------------------------------------------------------------------------------------------
cd ${SRCROOT}/capstool\nbbuild
call ant

@rem ------------------------------------------------------------------------------------------------------------
@rem Runtime
@rem ------------------------------------------------------------------------------------------------------------
cd ${alaska_sharedlibrary}/bdi
call ant
cd ${jbicomps_home}/nbbuild
call ant install

@rem ------------------------------------------------------------------------------------------------------------
@rem Findbugs
@rem ------------------------------------------------------------------------------------------------------------
@rem cd ${SRCROOT}
@rem call runfindbugs.bat
:end
