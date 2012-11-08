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
 * @(#)ParamMetedataCache.java
 *
 *
 *
 * END_HEADER - DO NOT EDIT
 */
package org.glassfish.openesb.databasebc.util;

import java.util.ArrayList;
import java.util.Hashtable;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantReadWriteLock;
import org.glassfish.openesb.databasebc.OperationMetaData;

/**
 *
 * @author Alexander Lomov, 2010
 */
public class ParamMetadataCache {

    private static ParamMetadataCache self_;
    private final static ReentrantReadWriteLock rwl = new ReentrantReadWriteLock(false); // Non-fair lock
    private final static Lock rl = rwl.readLock();
    private final static Lock wl = rwl.writeLock();
    private Hashtable<String, ParamMetadataHolder> params_;

    private ParamMetadataCache() {
        params_ = new Hashtable<String, ParamMetadataHolder>();
    }

    public static ParamMetadataCache instance() {
        rl.lock();
        if (self_ == null) {
            rl.unlock();
            wl.lock();
            if (self_ == null) {
                self_ = new ParamMetadataCache();
            }
            wl.unlock();
        } else {
            rl.unlock();
        }
        return self_;
    }

    public ArrayList<CachedQueryParameter> getMetadata(String connectionURL, OperationMetaData opMetadata) {
        String key = connectionURL;
        if (opMetadata.getJDBCSql() != null) { // it's prepared statement
            key = key.concat(opMetadata.getJDBCSql().getSql());
        } else {  // it's stored procedure call
            key = key.concat(opMetadata.getJDBCSPOperationInput().getExecutionString());
        }
        rl.lock();
        ArrayList<CachedQueryParameter> params = null;
        try {            
            ParamMetadataHolder holder = params_.get(key);
            if (holder != null && !holder.ShouldReset(opMetadata.hashCode())) {
                params = holder.getMetadata();
            }
        } finally {
            rl.unlock();
        }
        return params;

    }

    public void storeMetadata(String connectionURL, OperationMetaData opMetadata, ArrayList<CachedQueryParameter> params) {
        String key = connectionURL;
        if (opMetadata.getJDBCSql() != null) { // it's prepared statement        
            key = key.concat(opMetadata.getJDBCSql().getSql());
        } else { // it's stored procedure call        
            key = key.concat(opMetadata.getJDBCSPOperationInput().getExecutionString());
        }
        wl.lock();
        try{
            params_.put(key, new ParamMetadataHolder(params, opMetadata.hashCode()));
        } finally {
            wl.unlock();
        }
    }

    private class ParamMetadataHolder {

        private ArrayList<CachedQueryParameter> param_;
        private int opHash_;

        public ParamMetadataHolder(ArrayList<CachedQueryParameter> params, int opHashCode) {
            param_ = params;
            opHash_ = opHashCode;
        }

        public boolean ShouldReset(int opHashCode) {
            return opHash_ != opHashCode;
        }

        public ArrayList<CachedQueryParameter> getMetadata() {
            return param_;
        }
    }
}
