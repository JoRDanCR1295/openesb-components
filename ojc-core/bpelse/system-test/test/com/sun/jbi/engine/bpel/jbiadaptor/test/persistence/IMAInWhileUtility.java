package com.sun.jbi.engine.bpel.jbiadaptor.test.persistence;

import java.util.Properties;

import javax.xml.namespace.QName;

import com.sun.bpel.model.meta.RBPELProcess;
import com.sun.jbi.engine.bpel.DeploymentBindings;
import com.sun.jbi.engine.bpel.core.bpel.engine.Engine;
import com.sun.jbi.engine.bpel.core.bpel.engine.InComingEventModel;
import com.sun.jbi.engine.bpel.core.bpel.engine.MessageContainer;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimePartnerLink;
import com.sun.jbi.engine.bpel.jbiadaptor.test.common.EngineChannelSimulatorAdaptor;

public class IMAInWhileUtility extends UtilityClass {

    public void initiateBPInstanceForIMAInWhile(final Properties props, final Engine eng, 
            final DeploymentBindings deplBindings) 
            throws Exception {

        // initiate process
        initiateBPInstance(props, eng, deplBindings);
        // send message for second receive
        sendMessage("CORRELATING_", props, eng, deplBindings);
        sendMessage("CORRELATING_", props, eng, deplBindings);
        sendMessage("CORRELATING_", props, eng, deplBindings);
        sendMessage("CORRELATING_", props, eng, deplBindings);
    }

    public void associateIMAinWhileChannel(final Properties props, 
            final Engine eng, DeploymentBindings deplBindings) throws Exception {
        
        final InComingEventModel model = associateChannel(props, deplBindings);
        EngineChannelSimulatorAdaptor channel = new EngineChannelSimulatorAdaptor() {
            public Object invoke(MessageContainer msgContainer, RuntimePartnerLink partnerLink, QName operation, boolean oneWay, RBPELProcess process) {
                return acceptMessageForTest(props, msgContainer, model, eng);
            }
        };
        eng.setOutChannel(channel);
    }
}
