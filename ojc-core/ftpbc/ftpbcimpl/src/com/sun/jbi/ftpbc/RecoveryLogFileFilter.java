/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package com.sun.jbi.ftpbc;

import com.sun.jbi.ftpbc.persistence.FTPBCPersistStore;

import java.io.File;
import java.io.FileFilter;

/**
 * filter for recovery log entries
 * @author jfu
 */
public class RecoveryLogFileFilter implements FileFilter {

    public boolean accept(File pathname) {
        boolean b = false;
        if (pathname.isFile()) {
            String name = pathname.getName();
            if (name != null && name.endsWith(FTPBCPersistStore.RECOVER_LOG_SUFFIX)) {
                b = true;
            }
        }
        return b;
    }
}
