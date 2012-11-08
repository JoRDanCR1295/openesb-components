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
 * @(#)MasterTracker.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package org.glassfish.openesb.pojose.jbi.su;

import com.sun.jbi.common.qos.ServiceQuality;
import java.util.Iterator;
import java.util.List;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.locks.ReentrantLock;
import javax.jbi.messaging.MessageExchange;
import org.glassfish.openesb.pojose.jbi.thread.BaseTask;
import org.glassfish.openesb.pojose.jbi.thread.TaskThread;

/**
 * Also implements {@link ProviderTracker} to delegates to appropriate instance
 * of {@link ProviderTrackerImpl}
 * 
 * @author gpatil
 */
public class MasterTracker {
    // No need to declare as volatile.
    private static final MasterTracker instance = new MasterTracker();
    
    private final ConcurrentHashMap<String, ProviderTrackerImpl>  map =
            new ConcurrentHashMap<String, ProviderTrackerImpl>();
    private final ConcurrentHashMap<String, String>  consME2ProvME =
            new ConcurrentHashMap<String, String>();
    private final AtomicInteger activeSendSynchs = new AtomicInteger();
    // Map of Pojo class name and number of requests.
    private final ConcurrentHashMap<String, AtomicLong> activeInPojoSvcRequests =
            new ConcurrentHashMap<String, AtomicLong>();
    private ReentrantLock activeInPojoSvcReqLock = new ReentrantLock();

    private MasterTracker(){
    }

    public static MasterTracker getInstance(){
        return instance;
    }

    // *****                  *****
    // ***** Instance methods *****
    // *****                  *****
    
    public ProviderTracker getProviderTracker(MessageExchange provMe){
        ProviderTrackerImpl tracker = map.get(provMe.getExchangeId());
        if (tracker == null){
            // Keep outer get and if for performance reasons.
            synchronized(map){
                tracker = map.get(provMe.getExchangeId());

                if (tracker == null){
                    tracker = new ProviderTrackerImpl(this, provMe);
                    map.put(provMe.getExchangeId(), tracker);
                }
            }
        }
        return tracker;
    }

    void removeProviderTracker(MessageExchange me, List<String> consMsgExs){
        synchronized (map){
            this.map.remove(me.getExchangeId());
            Iterator<String> itr = consMsgExs.iterator();
            while (itr.hasNext()){
                String cme = itr.next();
                this.consME2ProvME.remove(cme);
                //System.out.println("Removed ConsME:" + cme);
            }
        }
    }

    private ProviderTracker getEndedServiceTracker(){
        return DummyProviderTracker.getInstance();
    }

    static String getConsMexId(MessageExchange consMe){
        String k = (String) consMe.getProperty(ServiceQuality.MESSAGE_ID);
        if ((k == null) || ("".equals(k.trim()))){
            k = consMe.getExchangeId();
        }

        return k;
    }

    public synchronized ProviderTracker getProviderTrackerForConsumerME(MessageExchange consMe){
        ProviderTracker pt = null;
        synchronized (map){
            String k = getConsMexId(consMe);
            
            String provMeId = this.consME2ProvME.get(k);
            if (provMeId != null){
                pt = this.map.get(provMeId);
                if (pt == null){
                    pt = getEndedServiceTracker();
                    //##### TODO Remove ME
                    //Exception ex = new Exception("PT is null!!!");
                    //ex.printStackTrace();
                }
            }else{
                //##### TODO Remove ME
                //Exception ex = new Exception("provMeId is null for ConsME:" + consMe.getExchangeId());
                //ex.printStackTrace();

                pt = getEndedServiceTracker();
            }
        }

        return pt;
    }

    public void addConsMsgEx(MessageExchange prov, MessageExchange cons){
        String k = getConsMexId(cons);
        this.consME2ProvME.put(k, prov.getExchangeId());
        //##### TODO Remove
        //System.out.println("Added consME:" + cons.getExchangeId());
        //##### TODO Remove -end
    }

    public void removeConsMsgEx(MessageExchange prov, MessageExchange cons){
        String k = getConsMexId(cons);
        this.consME2ProvME.remove(k);
        //##### TODO Remove
        //System.out.println("removeConsMsgEx: Removed consME:" + cons.getExchangeId());
        //##### TODO Remove - end
    }

// use only for stats purposes, Should not be used or care should be taken while
//    using for accurate just intime data.
    
//    public long getTaskThreadsCreated() {
//        return TaskThread.getTaskThreadsCreated();
//    }
//
//    public long getTaskThreadsRunning() {
//        return TaskThread.getTaskThreadsRunning();
//    }
//
//    public long getTaskThreadsCompleted() {
//        return TaskThread.getTaskThreadsCreated() - TaskThread.getTaskThreadsRunning();
//    }
//
//    public long getInMsgTaskCreated() {
//        return BaseTask.getInMsgTaskCreated();
//    }
//
//    public long getInMsgTaskCompleted() {
//        return BaseTask.getInMsgTaskCompleted();
//    }
//
//    public long getRespMsgTaskCreated() {
//        return BaseTask.getRespMsgTaskCreated();
//    }
//
//    public long getRespMsgTaskCompleted() {
//        return BaseTask.getRespMsgTaskCompleted();
//    }

    public void updateActiveInRequestForPojo(String pojoClsName, boolean increment){
        AtomicLong al = this.activeInPojoSvcRequests.get(pojoClsName);
        if (al == null){
            try {
                activeInPojoSvcReqLock.lock();
                if (al == null){
                    al = new AtomicLong();
                    al.set(0);
                }
            } finally {
                activeInPojoSvcReqLock.unlock();
            }
        }
        if (increment){
            al.incrementAndGet();
        } else {
            al.decrementAndGet();
        }
    }

    public long getActiveInRequestForPojo(String pojoClassName){
        AtomicLong al = this.activeInPojoSvcRequests.get(pojoClassName);
        if (al == null){
            return 0;
        } else {
            return al.longValue();
        }
    }
    
    public void updateActiveSynchCalls(boolean increment){
        if (increment){
            this.activeSendSynchs.incrementAndGet();
        } else {
            this.activeSendSynchs.decrementAndGet();
        }
    }

    public int getActiveSynchCalls(){
        return this.activeSendSynchs.intValue();
    }
}
