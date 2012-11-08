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
 * @(#)FileCache.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.component.toolkit.project.util;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import com.sun.jbi.common.util.Util;

/**
 * 
 * @author Kevan Simpson
 */
public class FileCache {
    // cache of file content, key: absolute path
    private Map<File, String> mContentMap;
    private Map<File, String> mDeltaMap;
    
    public FileCache() {
        mContentMap = new HashMap<File, String>();
        mDeltaMap = new HashMap<File, String>();
    }

    public void clearDeltas(File file) {
        // update cache with modified file content, then clear delta map
//        for (File f : mDeltaMap.keySet()) {
            mContentMap.put(file, mDeltaMap.remove(file));
//        }
//        mDeltaMap.clear();
    }
    
    public boolean isModified(File file) {
        return (file != null && mDeltaMap.get(file) != null);
    }
    
    public String getFileContent(File file) throws IOException {
        // look for changes first
        String content = mDeltaMap.get(file);
        
        // no changes, load from cache
        if (Util.isEmpty(content)) {
            content = mContentMap.get(file);
        }
        
        // cache not populated with this file, read from system
        if (Util.isEmpty(content)) {
            content = loadFile(file);
        }
        
        return content;
    }
    
    public void saveFileChanges(File file, String content) {
        if (file != null && !Util.isEmpty(content)) {
            mDeltaMap.put(file, content);
        }
    }

    private String loadFile(File file) throws IOException {
        String content = Util.readFileContent(file);
        // overwrites cache
        mContentMap.put(file, content);
        return content;
    }
}
