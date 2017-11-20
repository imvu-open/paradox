define("loader", ["util"], function(util) {
    //  The idea of the Loader is that we batch a bunch of counter gets
    //  into a single request, for better throughput.
    function Loader(currentDates) {
        this._nextEvent = null;
        this._nextEventTime = 0;
        this._getting = {};
        this._cbObjs = {};
        this._xhr = null;
        this._id = 0;
        this._currentDates = currentDates;
        this._events = {};
    }
    Loader.prototype.configureEvents = util.guard(function Loader_configureEvents(events) {
        this._events = events;
    })
    Loader.prototype.getEvents = util.guard(function Loader_getEvents() {
        return this._events;
    })
    Loader.prototype.addToNextGet = util.guard(function Loader_addToNextGet(keys, maxTime, maxSamples, cb) {
        this._id = this._id + 1;
        if (this._id > 1000000) {
            //  recycle IDs at some point
            this._id = 0;
        }
        var now = util.getAdjustedNow();
        var deadline = now.getTime() + maxTime;
        var cbObj = { keys: keys, cb: cb, id: this._id, maxSamples: maxSamples };
        this._cbObjs[cbObj.id] = cbObj;
        for (var k in keys) {
            var key = keys[k];
            if (!this._getting[key]) {
                this._getting[key] = [];
            }
            this._getting[key].push(cbObj);
        }
        if (this._nextEventTime > deadline) {
            clearTimeout(this._nextEvent);
            this._nextEvent = null;
            this._nextEventTime = 0;
        }
        if (this._nextEvent == null) {
            var self = this;
            this._nextEventTime = deadline;
            this._nextEvent = setTimeout(function() {
                self.onTimer();
            }, deadline - now.getTime());
        }
    })
    Loader.prototype.onTimer = util.guard(function Loader_onTimer() {
        this._nextEvent = null;
        this._nextEventTime = 0;
        if (this._xhr) {
            //  I'll do this when _xhr completes
            return;
        }
        this.startRequest();
    })
    Loader.prototype.startRequest = util.guard(function Loader_startRequest() {
        if (this._xhr) {
            throw new Error("Attempt to re-start Loader XMLHttpRequest()");
            return;
        }
        if (util.empty(this._getting)) {
            //  nothing to get -- we're done!
            return;
        }
        this._xhr = new XMLHttpRequest();
        var self = this;
        var gotten = this._getting;
        this._getting = {}
        var cbObjs = this._cbObjs;
        this._cbObjs = {}
        this._xhr.onreadystatechange = function () {
            if (self._xhr.readyState == 4) {
                self.onXhrComplete(gotten, cbObjs);
            }
        }
        var maxSamples = 1;
        for (var k in cbObjs) {
            if (cbObjs[k].maxSamples > maxSamples) {
                maxSamples = cbObjs[k].maxSamples;
            }
        }

        console.log(util.keys(gotten).length);

        this._xhr.open("POST", "/eval", true);
        this._xhr.setRequestHeader('Content-Type', 'application/json');
        var req = { start: Math.floor(this._currentDates.start.getTime() / 1000),
            stop: Math.floor(this._currentDates.stop.getTime() / 1000),
            keys: util.keys(gotten), // XXXll HACK
            maxSamples: maxSamples,
            events: _.map(this._events, function(el) { return { name: el.name, ident: el.ident, keys:el.keys} ; })
            };
        var reqstr = JSON.stringify(req);
        //  for debugging, pend the sending
        self._xhr.send(reqstr);
    })
    Loader.prototype.onXhrComplete = util.guard(function Loader_onXhrComplete(gotten, cbObjs) {
        var xhr = this._xhr;
        this._xhr = null;
        if (xhr.status > 299) {
            var res = JSON.parse(xhr.response);
            if ((typeof res) === "string") {
                resMsg = res.message.replace(/\\\\/g,"").replace(/\\n/g, "\r\n");
            } else {
                resMsg = JSON.stringify(res.message, null, 2);
            }
            if (res.data) {
                throw new Error("Error getting data from server:\n" + xhr.status + " " + xhr.statusText + "\n" + resMsg + "\n" + JSON.stringify(res.data, null, 2));
            }else{
                throw new Error("Error getting data from server:\n" + xhr.status + " " + xhr.statusText + "\n" + resMsg);
            }
        }
        else {
            var data = JSON.parse(xhr.responseText);
            console.log(data);
            util.closeErrorDialog();
            this.deliverData(data, gotten, cbObjs);
        }
        //  try reading whatever got queued next
        this.startRequest();
    })
    Loader.prototype.deliverData = function Loader_deliverData(data, gotten, cbObjs) {
        for (var k in cbObjs) {
            cbObjs[k].cb(data);
        }
    }
    return Loader;
});
