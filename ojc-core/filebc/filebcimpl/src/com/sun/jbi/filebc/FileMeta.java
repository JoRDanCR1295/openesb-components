/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)InputFileMeta.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.filebc;

import java.io.File;
import java.util.HashMap;
import java.util.Map;

/* 
 * Input file meta-data.
 * 
 */
public class FileMeta {
    // inbound file binding NM properties

    public static final String NMPROP_INBOUND_FILEDIR = "org.glassfish.openesb.file.inbound.filedirectory";
    public static final String NMPROP_INBOUND_FILENAME = "org.glassfish.openesb.file.inbound.filename";
    public static final String NMPROP_INBOUND_DATATYPE = "org.glassfish.openesb.file.inbound.datatype";
    public static final String NMPROP_INBOUND_BATCHID = "org.glassfish.openesb.file.inbound.batchid";
    public static final String NMPROP_INBOUND_RECORDNUMBER = "org.glassfish.openesb.file.inbound.recordnumber";
    public static final String NMPROP_INBOUND_LASTRECORD = "org.glassfish.openesb.file.inbound.lastrecord";
    public static final String NMPROP_INBOUND_ENDPOINTNAME = "org.glassfish.openesb.file.inbound.endPointname";
    // additional NM properties for inbound
    public static final String NMPROP_INBOUND_RELATIVE_PATH = "org.glassfish.openesb.file.inbound.relative.path";
    public static final String NMPROP_INBOUND_PATH_RELATIVE_TO = "org.glassfish.openesb.file.inbound.path.relativeto";
    public static final String NMPROP_INBOUND_PERSIST_BASE_DIR = "org.glassfish.openesb.file.inbound.persist.base.dir";
    public static final String NMPROP_INBOUND_LOCK_NAME = "org.glassfish.openesb.file.inbound.lock.file.name";
    public static final String NMPROP_INBOUND_LOCK_FILE = "org.glassfish.openesb.file.inbound.lock.file.path";
    public static final String NMPROP_INBOUND_WORK_AREA = "org.glassfish.openesb.file.inbound.work.area";
    public static final String NMPROP_INBOUND_WORK_AREA_DIR = "org.glassfish.openesb.file.inbound.work.area.dir";
    public static final String NMPROP_INBOUND_ARCHIVE_ENABLED = "org.glassfish.openesb.file.inbound.archive.enabled";
    public static final String NMPROP_INBOUND_ARCHIVE_AREA_RLATIVE = "org.glassfish.openesb.file.inbound.archive.area.relative";
    public static final String NMPROP_INBOUND_ARCHIVE_AREA = "org.glassfish.openesb.file.inbound.archive.area";
    public static final String NMPROP_INBOUND_ARCHIVED_FILE = "org.glassfish.openesb.file.inbound.archived.file";
    public static final String NMPROP_INBOUND_DEL_ON_READ = "org.glassfish.openesb.file.inbound.delete.on.read";
    public static final String NMPROP_INBOUND_FILE_IN_PROCESSING = "org.glassfish.openesb.file.inbound.file.in.processing";
    public static final String NMPROP_INBOUND_FILE_IN_ERROR = "org.glassfish.openesb.file.inbound.file.in.error";
    public static final String NMPROP_INBOUND_POLL_RECURSIVE = "org.glassfish.openesb.file.inbound.poll.recursive";
    public static final String NMPROP_INBOUND_POLL_EXCLUDE_REGEX = "org.glassfish.openesb.file.inbound.poll.exclude.regex";
    // outbound file binding NM properties
    public static final String NMPROP_OUTBOUND_FILEDIR = "org.glassfish.openesb.file.outbound.filedirectory";
    public static final String NMPROP_OUTBOUND_FILENAME = "org.glassfish.openesb.file.outbound.filename";
    public static final String NMPROP_OUTBOUND_DATATYPE = "org.glassfish.openesb.file.outbound.datatype";
    public static final String NMPROP_OUTBOUND_DIRRELATIVETO = "org.glassfish.openesb.file.outbound.diectoryrelativeto";
    public static final String NMPROP_OUTBOUND_ADDEOL = "org.glassfish.openesb.file.outbound.addeol";
    public static final String NMPROP_OUTBOUND_APPEND = "org.glassfish.openesb.file.outbound.append";
    public static final String NMPROP_OUTBOUND_APPENDDELIMITER = "org.glassfish.openesb.file.outbound.appenddelimiter";
    public static final String NMPROP_OUTBOUND_OVERWRITE = "org.glassfish.openesb.file.outbound.overwriteexistingfile";
    // additional NM properties for outbound
    // these NM properties are read only - meaning they are only
    // available after an outbound operation has completed
    // and they should be attached to the input message that provided
    // for the invoking of the outbound operation
    //
    // use cases:
    // the business logic might want to know the details of an outbound binding
    // such as :
    // (1) is the protect enabled and does the protect action occurred, where is the protected file be moved, etc.?
    // (2) is the staging enabled and where is the staged file, etc?
    // (3) is the sequence referenced in the outbound operation? what is the seq number? where is the persisted sequence file?
    //
    public static final String NMPROP_OUTBOUND_RELATIVE_PATH = "org.glassfish.openesb.file.outbound.relative.path";
    public static final String NMPROP_OUTBOUND_PERSIST_BASE_DIR = "org.glassfish.openesb.file.outbound.persist.base.dir";
    public static final String NMPROP_OUTBOUND_LOCK_NAME = "org.glassfish.openesb.file.outbound.lock.file.name";
    public static final String NMPROP_OUTBOUND_LOCK_FILE = "org.glassfish.openesb.file.outbound.lock.file.path";
    public static final String NMPROP_OUTBOUND_SEQ_AREA = "org.glassfish.openesb.file.outbound.seq.area";
    public static final String NMPROP_OUTBOUND_PROTECT_ENABLED = "org.glassfish.openesb.file.outbound.protect.enabled";
    public static final String NMPROP_OUTBOUND_PROTECT_AREA_RELATIVE = "org.glassfish.openesb.file.outbound.protect.area.path.relative";
    public static final String NMPROP_OUTBOUND_PROTECT_AREA = "org.glassfish.openesb.file.outbound.protect.area";
    public static final String NMPROP_OUTBOUND_PROTECTED_FILE = "org.glassfish.openesb.file.outbound.protected.file";
    public static final String NMPROP_OUTBOUND_STAGING_ENABLED = "org.glassfish.openesb.file.outbound.stage.enabled";
    public static final String NMPROP_OUTBOUND_STAGING_AREA_RELATIVE = "org.glassfish.openesb.file.outbound.stage.area.path.relative";
    public static final String NMPROP_OUTBOUND_STAGING_AREA = "org.glassfish.openesb.file.outbound.stage.area";
    public static final String NMPROP_OUTBOUND_STAGED_FILE = "org.glassfish.openesb.file.outbound.staged.file";
    public static final String NMPROP_GROUPID = "org.glassfish.openesb.messaging.groupid";
    public static final String NMPROP_MESSAGEID = "org.glassfish.openesb.messaging.messageid";
    public static final String NMPROP_OUTBOUND_ONDEMAND_READ_RECURSIVE = "org.glassfish.openesb.file.outbound.ondemand.read.recursive";
    public static final String NMPROP_OUTBOUND_ONDEMAND_READ_EXCLUDE_REGEX = "org.glassfish.openesb.file.outbound.ondemand.read.exclude.regex";
    private File inProcessFileHandle;
    private Map nmProperties;

    FileMeta(File inProcessFile) {
        this.inProcessFileHandle = inProcessFile;
        this.nmProperties = new HashMap();
    }

    public File getInProcessFileHandle() {
        return inProcessFileHandle;
    }

    public void setInProcessFileHandle(File workAreaFile) {
        this.inProcessFileHandle = workAreaFile;
    }

    public Map getNMProperties() {
        return nmProperties;
    }

    public void setNMProperties(Map nmProperties) {
        this.nmProperties = nmProperties;
    }

    public void setNMProperty(String key, String value) {
        nmProperties.put(key, value);
    }

    public String getNMProperty(String key) {
        return (String) nmProperties.get(key);
    }

    public void setNMProperties(String[] keys, String[] values) {
        for (int i = 0; i < keys.length; i++) {
            nmProperties.put(keys[i], values[i]);
        }
    }
}
