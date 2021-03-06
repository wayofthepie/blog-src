---
title: Some JS and Haskell stuff
tags: haskell, js
---

# Vulgr UI - React
Vulgr is a project I've been working on, on and off, for a while. The goal is to build a
dependency analysis platform - analyze your `gradle`/`node`/`whatever` dependencies to see
whether they contain known vulnerabilities or bugs, reading from whatever data sources (CVE
database, github issues, etc...) that make sense.

This week, I did some work on how this information could be displayed to users on a web
interface, e.g.:

<img src="/images/vulgr-ui-jan3-weekly.png" class="img-responsive" />

This is built with [react](https://facebook.github.io/react/) and [material-ui](http://www.material-ui.com/#/).
The code for the card is located at
[AnalysisSummaryCard.js](https://github.com/wayofthepie/vulgr-ui-experimental/blob/material/src/components/analysis/summary/AnalysisSummaryCard.js).
Following is the code for the status icon's in the card, nice, flexible re-usable component!

```{.javascript}
'use strict';

import React, {PropTypes} from 'react';
import ReactTooltip from 'react-tooltip';

let statusIcon = (iconType, attributes, style) => {
  return (
    <i className='material-icons md-36' {...attributes}
       style={style}>
      {iconType}
    </i>
  );
};

let tooltippableStatusIcon = (iconType, tooltipInfo, style) => {
  let tooltip = function () {
    if (tooltipInfo) {
      return (
        <ReactTooltip place='top'
                      id={tooltipInfo.id}
                      type={tooltipInfo.type}
                      effect='solid'>
          <span ><strong>{tooltipInfo.message}</strong></span>
        </ReactTooltip>
      );
    } else {
      return null;
    }
  };

  let tooltippedAttributes = () => {
    return tooltipInfo ? {'data-tip': '', 'data-for': tooltipInfo.id} : [];
  };

  return (
    <div>
      { tooltip() }
      { statusIcon(iconType, tooltippedAttributes(), style) }
    </div>
  );
};

export const StatusIcon = ({iconType, tooltipInfo, style}) => {
  return tooltippableStatusIcon(iconType, tooltipInfo, style);
};

StatusIcon.propTypes = {
  iconType: PropTypes.string.isRequired,
  tooltipInfo: PropTypes.shape({
    id: PropTypes.string.isRequired,
    type: PropTypes.string.isRequired,
    message: PropTypes.string.isRequired
  }),
  style: PropTypes.object
};

export default StatusIcon;
```

Here is an example of its use to render the error icon, first a function to wrap up the JSX:

```javascript
let createStatusIcon = (iconType, tooltipInfo, style) => {
  return (
    <StatusIcon
      iconType={iconType}
      tooltipInfo={tooltipInfo}
      style={style} />
  );
};
```

Concrete usage:
```javascript
let errorIcon = () => {
  let icon = 'error';
  let marginLeft = 10;
  let tooltipInfo = buildTooltipInfo('analysis-card-error-tooltip', 'error', 'Error!', {marginLeft});
  let style = {color: 'red', float: 'left', marginLeft};
  return createStatusIcon(icon, tooltipInfo, style);
};
```

Pretty flexible! The color and style can easily be changed so the icon can fit in without
much hassle in components with complex styles, or can be greyed out, etc...

I use Angular and bootstrap at work, so far _react_ and _material_ have been a dream in
comparison.

# Haskell Criu RPC Client
I've been toying about with [criu](https://criu.org/) for the last few weeks. I decided to
write some bindings to it in haskell.

Criu's RPC API [^1] uses [protobuf](https://github.com/google/protobuf). I used the
[proto-lens](https://github.com/google/proto-lens) library to generate lenses and data
types. This gives a pretty nice foundation to build upon - the project for generation is on
github, [haskell-criu-rpc-types](https://github.com/wayofthepie/haskell-criu-rpc-types)
[^2].

With that foundation, I threw together a quick client:

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Criu (
  module Proto.Criu.Rpc
  , module Lens.Family2
  , callCriu
  , callCriu'
  ) where

import Control.Exception.Base (IOException, bracket, try)
import Lens.Family2 ((.~))
import Data.ProtoLens (decodeMessage, encodeMessage)
import Proto.Criu.Rpc
import Network.Socket (Family(AF_UNIX), SocketType(SeqPacket), SockAddr(SockAddrUnix), close, connect, socket)
import Network.Socket.ByteString (recv, send)

-- | Send request to criu socket. Can throw exceptions.
callCriu :: FilePath -> Criu_req -> IO (Either String Criu_resp)
callCriu fp req = do
  resp <- withSocket $ \sock -> do
    connect sock (SockAddrUnix fp)
    send sock (encodeMessage req)
    recv sock 1024
  pure (decodeMessage resp :: Either String Criu_resp)
 where
  withSocket f = bracket
    (socket AF_UNIX SeqPacket 0)
    (close)
    (\sock -> f sock)

-- | Send a request to criu, but wrap up IOExceptions in Either.
callCriu' :: FilePath -> Criu_req -> IO (Either String Criu_resp)
callCriu' fp req = do
  eitherResp <- try (callCriu fp req)
  case eitherResp of
    Right resp -> pure resp
    Left (e :: IOException) -> pure . Left . show $ e
```
Both `callCriu` and `callCriu'` expect the path to the `criu` socket and a `Criu_req` type.

To build the actual `Criu_req` expected by the calls is is pretty straightforward. In
`ghci`:

```haskell
> build (type' .~ CHECK) :: Criu_req
Criu_req {
  _Criu_req'type' = CHECK
  , _Criu_req'opts = Nothing
  , _Criu_req'notifySuccess = Nothing
  , _Criu_req'keepOpen = Nothing
  , _Criu_req'features = Nothing
  }
```
`build (type' .~ CHECK) :: Criu_req` builds a request for a `criu` check. So, an actual call
looks as follows:

```haskell
> callCriu' "criu_service.socket" (build (type' .~ CHECK) :: Criu_req)
Right (
  Criu_resp {
    _Criu_resp'type' = CHECK
    , _Criu_resp'success = True
    , _Criu_resp'dump = Nothing
    , _Criu_resp'restore = Nothing
    , _Criu_resp'notify = Nothing
    , _Criu_resp'ps = Nothing
    , _Criu_resp'crErrno = Nothing
    , _Criu_resp'features = Nothing
    , _Criu_resp'crErrmsg = Nothing
    }
  )
```
From the service logs:

```
$ criu service
Warn  (cr-service.c:1023): Binding to local dir address!
Warn  (cr-check.c:827): Skipping cgroup namespaces check
Looks good.
```
Success! Lots to improve, but a good start. I'll do a more in-depth post on this once it
matures.

[^1]: See <https://criu.org/RPC> for more information.
[^2]: It's also on hackage, <https://hackage.haskell.org/package/criu-rpc-types-0.0.0.1>,
however I'm not sure what the standard is for libraries that generate their code.
