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
 * @(#)MSMQXAResourceImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.msmqbc.transaction;

import javax.transaction.xa.Xid;
import javax.transaction.xa.XAResource;
import javax.transaction.xa.XAException;

import java.util.logging.Logger;
import java.util.logging.Level;

import java.io.Serializable;

import com.sun.jbi.internationalization.Messages;

/**
 * XAResource implementation for MSMQ BC
 * 
 * @author Sun Microsystems
 */

public class MSMQXAResourceImpl implements XAResource, Serializable {

    private static final Logger mLogger = Messages.getLogger(MSMQXAResourceImpl.class);

    private static final Messages mMessages = Messages.getMessages(MSMQXAResourceImpl.class);

    private static final long serialVersionUID = 3256727264572813369L;

    public String xa_resource_name = new String("Tm=SUN,RmRecoveryGuid=7A9A2F35-F3D1-4907-AD53-623DAB88F718");

    private int timeout = 0;;

    private int nXids = 30;

    // the follwing number has to be same as our jts will do
    private int gtridlength = 24;

    private int bquallength = 18;

    private MSMQXAResourceWrap myMSMQXAResourceWrap;

    public MSMQXAResourceImpl() {

        mLogger.log(Level.INFO, "MSDTCXARESOURCE.INIT_CALLED");

        myMSMQXAResourceWrap = new MSMQXAResourceWrap();

        myMSMQXAResourceWrap.open(this.xa_resource_name);
    }

    public MSMQXAResourceImpl(String guid) {
        String xa_resource = new String("Tm=SUN,RmRecoveryGuid=" + guid);

        mLogger.log(Level.INFO, "MSDTCxaResource.INIT_GUID_CALLED", new Object[] { guid });

        myMSMQXAResourceWrap = new MSMQXAResourceWrap();

        myMSMQXAResourceWrap.open(xa_resource_name);
    }

    public void commit(Xid xid, boolean flag) throws XAException {
        String gTrid = new String(xid.getGlobalTransactionId());
        String bQual = new String(xid.getBranchQualifier());
        int formatId = xid.getFormatId();
        byte gtrid[] = xid.getGlobalTransactionId();
        byte bqual[] = xid.getBranchQualifier();

        mLogger.log(Level.INFO, "MSDTCXARESOURCE.COMMIT_GUID_AND_FLAG_VAL", new Object[] { xid.toString(), flag });
        mLogger.log(Level.INFO, "MSDTCXARESOURCE.COMMIT_GTRID_AND_BQUAL_VAL", new Object[] { gTrid, bQual, formatId });
        mLogger.log(Level.INFO, "MSDTCXARESOURCE.COMMIT_GUID_AND_FLAG_VAL", new Object[] { xid.MAXBQUALSIZE,
                xid.MAXGTRIDSIZE });

        // mLogger.info("MSDTCxaResource.xa_commit, gtrid, bqual");
        // printByteArray(gtrid);
        // printByteArray(bqual);

        myMSMQXAResourceWrap.commit(gtrid, bqual, formatId, flag);
        // mLogger.info("xa_commit, gTrid " + gTrid + " bQual " + bQual + " formatId " + formatId);
    }

    public void end(Xid xid, int flags) throws XAException {
        String gTrid = new String(xid.getGlobalTransactionId());
        String bQual = new String(xid.getBranchQualifier());
        int formatId = xid.getFormatId();
        byte gtrid[] = xid.getGlobalTransactionId();
        byte bqual[] = xid.getBranchQualifier();
        mLogger.log(Level.INFO, "MSDTCXARESOURCE.END_XID_FLAGS", new Object[] { xid.toString(), flags });
        mLogger.log(Level.INFO, "MSDTCXARESOURCE.END_GTRID_BQUAL_FORMATID", new Object[] { gTrid, bQual, formatId });
        mLogger.log(Level.INFO, "MSDTCXARESOURCE.END_MAXBQUALSIZE_MAXGTRIDSIZE", new Object[] { xid.MAXBQUALSIZE,
                xid.MAXGTRIDSIZE });
        // mLogger.info("MSDTCxaResource.xa_end, gtrid, bqual");
        // printByteArray(gtrid);
        // printByteArray(bqual);

        myMSMQXAResourceWrap.end(gtrid, bqual, formatId, flags);
        // mLogger.info("xa_end, gTrid " + gTrid + " bQual " + bQual + " formatId " + formatId);
    }

    public void forget(Xid xid) throws XAException {
        String gTrid = new String(xid.getGlobalTransactionId());
        String bQual = new String(xid.getBranchQualifier());
        int formatId = xid.getFormatId();
        byte gtrid[] = xid.getGlobalTransactionId();
        byte bqual[] = xid.getBranchQualifier();
        mLogger.log(Level.INFO, "MSDTCXARESOURCE.FORGET_XID", new Object[] { xid.toString() });
        mLogger.log(Level.INFO, "MSDTCXARESOURCE.FORGET_GTRID_BQUAL_FORMATID", new Object[] { gTrid, bQual, formatId });
        mLogger.log(Level.INFO, "MSDTCXARESOURCE.FORGET_MAXBQUALSIZE_MAXGRIDSIZE", new Object[] { xid.MAXBQUALSIZE,
                xid.MAXGTRIDSIZE });
        // mLogger.info("MSDTCxaResource.xa_forget, gtrid, bqual");
        // printByteArray(gtrid);
        // printByteArray(bqual);

        myMSMQXAResourceWrap.forget(gtrid, bqual, formatId);
        // mLogger.info("xa_forget, gTrid " + gTrid + " bQual " + bQual + " formatId " + formatId);
    }

    public int getTransactionTimeout() throws XAException {
        mLogger.log(Level.INFO, "MSDTCXARESOURCE.GET_TX_TIME_OUT");
        return timeout;
    }

    public boolean isSameRM(XAResource xares) {
        XAResource thisXAResource = (XAResource) this;

        if (thisXAResource == xares) {
            mLogger.log(Level.INFO, "MSDTCXARESOURCE.IS_SAME_RM_TRUE");
            return true;
        } else {
            mLogger.log(Level.INFO, "MSDTCXARESOURCE.IS_SAME_RM_FALSE");
            return false;
        }
    }

    public int prepare(Xid xid) throws XAException {
        String gTrid = new String(xid.getGlobalTransactionId());
        String bQual = new String(xid.getBranchQualifier());
        int formatId = xid.getFormatId();
        byte gtrid[] = xid.getGlobalTransactionId();
        byte bqual[] = xid.getBranchQualifier();
        mLogger.log(Level.INFO, "MSDTCXARESOURCE.PREPARE_XID", new Object[] { xid.toString() });
        mLogger.log(Level.INFO, "MSDTCXARESOURCE.PREPARE_GTRID_BQUAL_FORMATID", new Object[] { gTrid, bQual, formatId });
        mLogger.log(Level.INFO, "MSDTCXARESOURCE.PREPARE_MAXBQUALSIZE_MAXGRIDSIZE", new Object[] { xid.MAXBQUALSIZE,
                xid.MAXGTRIDSIZE });
        // mLogger.info("MSDTCxaResource.xa_prepare, gtrid, bqual");
        // printByteArray(gtrid);
        // printByteArray(bqual);

        myMSMQXAResourceWrap.prepare(gtrid, bqual, formatId);

        // mLogger.info("xa_prepare, gTrid " + gTrid + " bQual " + bQual + " formatId " + formatId);
        return 0;
    }

    public Xid[] recover(int flags) throws XAException {

        // Xid[] abasexid = new Xid[nXids];
        byte[] gtrids = new byte[nXids * gtridlength];
        byte[] bquals = new byte[nXids * bquallength];
        byte[] gTrid = new byte[gtridlength];
        byte[] bQual = new byte[bquallength];
        // byte[] bQual = {'1','2','3','4','5','6','7','8'};
        int theseXids = 0;

        mLogger.log(Level.INFO, "MSDTCXARESOURCE.RECOVER_FLAGS", new Object[] { flags });

        theseXids = myMSMQXAResourceWrap.recover(gtrids, bquals, (long) nXids, flags);

        mLogger.log(Level.INFO, "MSDTCXARESOURCE.RECOVER_AFTER_CALL_WRAP_RECOVER", new Object[] { theseXids });

        if (theseXids < 0) {
            XAException xae = new XAException();
            xae.errorCode = xae.XAER_RMFAIL;
            mLogger.log(Level.INFO, "MSDTCXARESOURCE.RECOVER_RETURN_BY_EX");
            throw xae;
        }

        Xid[] abasexid = new Xid[theseXids];

        for (int i = 0; i < theseXids; i++) {
            for (int j = 0; j < gtridlength; j++) {
                gTrid[j] = gtrids[i * gtridlength + j];
            }
            for (int k = 0; k < bquallength; k++) {
                bQual[k] = bquals[i * bquallength + k];
            }

            // abasexid[i] = TxHelper.createXid(gTrid, bQual);

            mLogger.log(Level.INFO, "MSDTCXARESOURCE.RECOVER_ITERATE", new Object[] { i, gTrid, bQual });
        }

        mLogger.log(Level.INFO, "MSDTCXARESOURCE.RECOVER_RETURNING");

        return abasexid;
    }

    public void rollback(Xid xid) throws XAException {
        String gTrid = new String(xid.getGlobalTransactionId());
        String bQual = new String(xid.getBranchQualifier());
        int formatId = xid.getFormatId();
        boolean flag = false;
        int flags = 0;

        byte gtrid[] = xid.getGlobalTransactionId();
        byte bqual[] = xid.getBranchQualifier();
        mLogger.log(Level.INFO, "MSDTCXARESOURCE.ROLL_BACK_XID", new Object[] { xid.toString() });
        mLogger.log(Level.INFO, "MSDTCXARESOURCE.ROLL_BACK_GTRID_BQUAL_FORMATID",
                new Object[] { gTrid, bQual, formatId });
        mLogger.log(Level.INFO, "MSDTCXARESOURCE.ROLL_BACK_MAXBQUALSIZE_MAXGTRIDSIZE", new Object[] { xid.MAXBQUALSIZE,
                xid.MAXGTRIDSIZE });

        // mLogger.info("MSDTCxaResource.xa_rollback, gtrid, bqual");
        // printByteArray(gtrid);
        // printByteArray(bqual);

        myMSMQXAResourceWrap.rollback(gtrid, bqual, formatId);

        // mLogger.info("xa_rollback, gTrid " + gTrid + " bQual " + bQual + " formatId " +
        // formatId);
    }

    public boolean setTransactionTimeout(int i) throws XAException {
        mLogger.log(Level.INFO, "MSDTCXARESOURCE.SET_TX_TIME_OUT_CALLED", new Object[] { i });

        if (i < 0) {
            throw new XAException(-5);
        } else {
            timeout = i;
            return false;
        }
    }

    public void start(Xid xid, int flags) throws XAException {
        String gTrid = new String(xid.getGlobalTransactionId());
        String bQual = new String(xid.getBranchQualifier());
        int formatId = xid.getFormatId();
        byte gtrid[] = xid.getGlobalTransactionId();
        byte bqual[] = xid.getBranchQualifier();

        mLogger.log(Level.INFO, "MSDTCXARESOURCE.START_XID_FLAGS", new Object[] { xid.toString(), flags });
        mLogger.log(Level.INFO, "MSDTCXARESOURCE.START_GTRID_BQUAL_FORMATID", new Object[] { gTrid, bQual, formatId });
        mLogger.log(Level.INFO, "MSDTCXARESOURCE.START_MAXBQUALSIZE_MAXGRIDSIZE", new Object[] { xid.MAXBQUALSIZE,
                xid.MAXGTRIDSIZE });
        myMSMQXAResourceWrap.start(gtrid, bqual, formatId, flags);

        // System.out.println("xa_start, gTrid " + gTrid + " bQual " + bQual + " formatId " +
        // formatId);
    }

    public void close() {
        // String xa_resource_name = new
        // String("Tm=TUXEDO,RmRecoveryGuid=7A9A2F35-F3D1-4907-AD53-623DAB88F718");
        mLogger.log(Level.INFO, "MSDTCXARESOURCE.CLOSE");

        myMSMQXAResourceWrap.close(xa_resource_name);
        // myMSMQXAResourceWrap.remove();
        // myMSMQXAResourceWrap = null;
    }

    public int getInstance() {
        mLogger.log(Level.INFO, "MSDTCXARESOURCE.GET_INSTANCE");
        return myMSMQXAResourceWrap.getInstance();
    }

    public String toString() {
        mLogger.log(Level.INFO, "MSDTCXARESOURCE.TO_STRING", new Object[] { xa_resource_name + this.hashCode() });
        return xa_resource_name + this.hashCode();
    }

    /*
     * private void printByteArray(byte bArray[]) { mLogger.info("length = " + bArray.length);
     * for(int i = 0; i < bArray.length; i++) mLogger.info(bArray[i] + " "); System.out.println(); }
     */

}
