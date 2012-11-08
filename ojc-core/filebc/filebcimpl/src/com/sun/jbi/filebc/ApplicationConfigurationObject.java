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
 * @(#)RuntimeConfiguration.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.filebc;

import java.util.Map;
import java.util.StringTokenizer;

import javax.management.openmbean.CompositeData;

public class ApplicationConfigurationObject {
    // names

    public static final String FILEDIR = "fileDirectory";
    public static final String RELATIVEPATH = "relativePath";
    public static final String PATHRELATIVETO = "pathRelativeTo";
    public static final String LOCKNAME = "lockName";
    public static final String WORKAREA = "workArea";
    public static final String SEQNAME = "seqName";
    public static final String PERSISTBASELOC = "persistenceBaseLoc";
    public static final String RECURSIVE = "recursive";
    public static final String EXCLUDE_REGEX = "recursiveExclude";
    public static final String FILEDIR_DESC = "File Directory";
    public static final String RELATIVEPATH_DESC = "Is directory relative path";
    public static final String PATHRELATIVETO_DESC = "Root for relative path";
    public static final String LOCKNAME_DESC = "Lock Name";
    public static final String WORKAREA_DESC = "Work Area";
    public static final String SEQNAME_DESC = "Seq Name";
    public static final String PERSIST_BASE_LOC_DESC = "Persistence Base Location";
    public static final String RECURSIVE_DESC = "Poll or Fetch Recursively";
    public static final String EXCLUDE_REGEX_DESC = "Exclude Entries Regex";
    // descs
    private String fileDirectory = "";
    private Boolean relativePath = Boolean.FALSE;
    private String pathRelativeTo = "";
    private String lockName = Lock.DEFAULT_INBOUND_LOCKFILE_NAME;
    private String workArea = Lock.DEFAULT_INBOUND_TMPDIR_NAME;
    private String seqName = Lock.DEFAULT_SEQFILE_NAME;
    private String persistBaseLoc = "";
    private String excludeRegex = "";
    private Boolean recursive = Boolean.FALSE;

    public ApplicationConfigurationObject() {
    }

    public ApplicationConfigurationObject(String appConfigString) {
        parse(appConfigString);
    }

    public ApplicationConfigurationObject(Map parms) {
        fileDirectory = (String) parms.get(FILEDIR);
        relativePath = (Boolean) parms.get(RELATIVEPATH);
        pathRelativeTo = (String) parms.get(PATHRELATIVETO);
        lockName = (String) parms.get(LOCKNAME);
        workArea = (String) parms.get(WORKAREA);
        seqName = (String) parms.get(SEQNAME);
        persistBaseLoc = (String) parms.get(PERSISTBASELOC);
        recursive = (Boolean) parms.get(RECURSIVE);
        excludeRegex = (String) parms.get(EXCLUDE_REGEX);
    }

    public ApplicationConfigurationObject(CompositeData appConfig, ApplicationConfigurationObject aco) {
        fileDirectory = (appConfig.get(FILEDIR) != null) ? (String) appConfig.get(FILEDIR) : aco != null ? aco.getFileDirectory() : "";
        //relativePath = Boolean.valueOf((String)appConfig.get(RELATIVEPATH));
        relativePath = appConfig.get(RELATIVEPATH) != null ? (Boolean) appConfig.get(RELATIVEPATH) : aco != null ? aco.getRelativePath() : new Boolean(false);
        String rootPath = (String) appConfig.get(PATHRELATIVETO);
        pathRelativeTo = (rootPath != null && rootPath.length() > 0) ? (String) appConfig.get(PATHRELATIVETO) : aco != null ? aco.getPathRelativeTo() : "";
        String lck = (String) appConfig.get(LOCKNAME);
        lockName = (lck != null && lck.length() > 0) ? lck : Lock.DEFAULT_INBOUND_LOCKFILE_NAME;
        String tmpdir = (String) appConfig.get(WORKAREA);
        workArea = (tmpdir != null && tmpdir.length() > 0) ? tmpdir : Lock.DEFAULT_INBOUND_TMPDIR_NAME;
        String seq = (String) appConfig.get(SEQNAME);
        seqName = (seq != null && seq.length() > 0) ? seq : Lock.DEFAULT_SEQFILE_NAME;
        String persistBase = (String) appConfig.get(PERSISTBASELOC);
        persistBaseLoc = (persistBase != null && persistBase.length() > 0) ? persistBase : "";
        recursive = appConfig.get(RECURSIVE) != null ? (Boolean) appConfig.get(RECURSIVE) : aco != null ? aco.getRecursive() : new Boolean(false);
        String exclude = (String) appConfig.get(EXCLUDE_REGEX);
        excludeRegex = (exclude != null && exclude.length() > 0) ? exclude : "";
    }

    public String getFileDirectory() {
        return fileDirectory;
    }

    public void setFileDirectory(String fileDirectory) {
        this.fileDirectory = fileDirectory;
    }

    public Boolean getRelativePath() {
        return relativePath;
    }

    public void setRelativePath(Boolean relativePath) {
        this.relativePath = relativePath;
    }

    public String getPathRelativeTo() {
        return pathRelativeTo;
    }

    public void setPathRelativeTo(String pathRelativeTo) {
        this.pathRelativeTo = pathRelativeTo;
    }

    public String getLockName() {
        return lockName;
    }

    public void setLockName(String lockName) {
        this.lockName = lockName;
    }

    public String getWorkArea() {
        return workArea;
    }

    public void setWorkArea(String workArea) {
        this.workArea = workArea;
    }

    public String getSeqName() {
        return seqName;
    }

    public void setSeqName(String seqName) {
        this.seqName = seqName;
    }

    public String getPersistenceBaseLoc() {
        return persistBaseLoc;
    }

    public void setPersistenceBaseLoc(String persistLoc) {
        this.persistBaseLoc = persistLoc;
    }

    public String getRecursiveExclude() {
        return excludeRegex;
    }

    public void setRecursiveExclude(String s) {
        this.excludeRegex = s;
    }

    public Boolean getRecursive() {
        return recursive;
    }

    public void setRecursive(Boolean b) {
        this.recursive = b;
    }

    public String toString() {
        StringBuffer strbuf = new StringBuffer();
        strbuf.append(FILEDIR + "{" + fileDirectory == null ? " " : fileDirectory + "}");
        strbuf.append(RELATIVEPATH + "{" + relativePath + "}");
        strbuf.append(PATHRELATIVETO + "{" + pathRelativeTo == null ? " " : pathRelativeTo + "}");
        strbuf.append(LOCKNAME + "{" + lockName == null ? " " : lockName + "}");
        strbuf.append(WORKAREA + "{" + workArea == null ? " " : workArea + "}");
        strbuf.append(SEQNAME + "{" + seqName == null ? " " : seqName + "}");
        strbuf.append(PERSISTBASELOC + "{" + persistBaseLoc == null ? " " : persistBaseLoc + "}");
        strbuf.append(RECURSIVE + "{" + recursive + "}");
        strbuf.append(EXCLUDE_REGEX + "{" + excludeRegex == null ? " " : excludeRegex + "}");

        return strbuf.toString();
    }

    private void parse(String str) {
        StringTokenizer st = new StringTokenizer(str, "{}");
        while (st.hasMoreTokens()) {
            String token = st.nextToken();
            if (FILEDIR.equals(token)) {
                fileDirectory = st.nextToken();
                if (fileDirectory != null || fileDirectory.trim().length() == 0) {
                    fileDirectory = fileDirectory.trim();
                }
            } else if (RELATIVEPATH.equals(token)) {
                relativePath = Boolean.valueOf(st.nextToken());
            } else if (PATHRELATIVETO.equals(token)) {
                pathRelativeTo = st.nextToken();
                if (pathRelativeTo != null || pathRelativeTo.trim().length() == 0) {
                    pathRelativeTo = pathRelativeTo.trim();
                }
            } else if (LOCKNAME.equals(token)) {
                lockName = st.nextToken();
                if (lockName != null || lockName.trim().length() == 0) {
                    lockName = lockName.trim();
                }
            } else if (WORKAREA.equals(token)) {
                workArea = st.nextToken();
                if (workArea != null || workArea.trim().length() == 0) {
                    workArea = workArea.trim();
                }
            } else if (SEQNAME.equals(token)) {
                seqName = st.nextToken();
                if (seqName != null || seqName.trim().length() == 0) {
                    seqName = seqName.trim();
                }
            } else if (PERSISTBASELOC.equals(token)) {
                persistBaseLoc = st.nextToken();
                if (persistBaseLoc != null || persistBaseLoc.trim().length() == 0) {
                    persistBaseLoc = persistBaseLoc.trim();
                }
            } else if (RECURSIVE.equals(token)) {
                recursive = Boolean.valueOf(st.nextToken());
            } else if (EXCLUDE_REGEX.equals(token)) {
                excludeRegex = st.nextToken();
                if (excludeRegex != null || excludeRegex.trim().length() == 0) {
                    excludeRegex = excludeRegex.trim();
                }
            }
        }
    }

    /*
     * escape special characters in the given string
     */
    private String cleanValue(String str) {
        if (str == null || str.length() == 0) {
            return str;
        }

        int i = str.indexOf('\\');
        if ((i < 0)) {
            return str;
        }

        if (i + 1 == str.length()) {
            return str.replaceAll("\\\\", "\\\\\\\\");
        }

        if (str.charAt(i + 1) == '\\') {
            // TODO: handle this better
            // with regex. Check the entire
            // string rather than just the first
            // appreance of '\'.
            return str;
        }

        // escape all '\' characters.
        return str.replaceAll("\\\\", "\\\\\\\\");
    }
}
