<?xml version="1.0" encoding="utf-8"?>
<p:config xmlns:p="http://www.orbeon.com/oxf/pipeline"
    xmlns:oxf="http://www.orbeon.com/oxf/processors"
    xmlns:sun="http://my.sun.com/ops/processors"
    xmlns:delegation="http://orbeon.org/oxf/xml/delegation">
<p:param name="instance" type="input"/>
<!--<p:param name="data" type="output"/>-->

<p:processor name="oxf:request">
<p:input name="config">
<config>
    <include>/request/parameters/parameter[name = 'taskId']</include>
    <include>/request/parameters/parameter[name = 'userId']</include>
    <include>/request/parameters/parameter[name = 'claimedBy']</include></config>
</p:input>
<p:output name="data" id="request"/>
</p:processor>

<p:processor name="sun:processor">
    <p:input name="request" href="#request"/>
    <p:input name="instance" href="#instance"/>
    <p:output name="data" id="result" debug="true"/>
</p:processor>


<p:processor name="oxf:redirect">
    <p:input name="data" href="#result"/>
</p:processor>
</p:config>
