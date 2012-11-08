/*
 * ProviderSEComponentLifeCycle.java
 */
package xacmlse;

import com.sun.jbi.sample.component.common.BasicComponentLifeCycle;
import com.sun.jbi.sample.component.common.DefaultMessageExchangeReceiver;
import com.sun.jbi.sample.component.common.MessageExchangeReceiver;
import com.sun.jbi.sample.component.common.RuntimeContext;
import javax.jbi.JBIException;
import javax.jbi.component.Component;

/**
 * This class extends the basic ComponentLifeCycle implementation to provide component
 * specific implementation of the ComponentLifeCycle.
 *
 * @see javax.jbi.ComponentLifeCycle
 * @see com.sun.jbi.sample.component.common.BasicComponentLifeCycle
 * @author chikkala
 */
public class ProviderSEComponentLifeCycle extends BasicComponentLifeCycle {
    
    /** constructor */
    public ProviderSEComponentLifeCycle(Component compRuntime) {
        super(compRuntime);
    }
    /**
     * creates DefaultMessageExchangeReceiver to handles receiving and processing
     * the message exchanges from the delivery channel.
     */
    @Override
    protected MessageExchangeReceiver createMessageExchangeReceiver() {
        return new DefaultMessageExchangeReceiver();
    }
    /**
     * chance to extended classes to do the component specific init
     * @throws javax.jbi.JBIException
     */
    @Override
    protected void doInit() throws JBIException {
        // NOOP
        RuntimeContext.getInstance().setLogger(this.getClass().getName(), null);
    }
    @Override
    protected void doStart() throws JBIException{
    /*
     try {
            this.mChannel= this.mContext.getDeliveryChannel();
            this.mMsgReceiver = new SEMsgReceiver(this.mChannel,this.mContext);
            this.mMsgReceiver.mServiceUnitMgr = this.mServiceUnitMgr;
            //Thread pool management would be here.
            // sblais 27th feb 2006
            Thread recThrd = new Thread(this.mMsgReceiver);
            recThrd.start();
        } catch (MessagingException me){
            System.out.println(me.getLocalizedMessage());
            me.printStackTrace();
        } catch (JBIException je){
            System.out.println(je.getLocalizedMessage());
            je.printStackTrace();
           */ 
        }

    }
