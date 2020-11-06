const url = require(`url`);

exports.http = require(`http`);

exports.https = require(`https`);

exports.protocolImpl = Nothing => Just => url => {
	try {
		return Just(new URL(url).protocol);
	} catch (_) {
		return Nothing;
	}
};

exports.requestImpl = module => request => aC => xC => () => {
	const req = module.request(
		request.url,
		{
			method: request.method,
			headers: request.headers,
			timeout: request.timeout ? request.timeout : undefined,
		},
		res => {
			res.on(`error`, xC);

			const buffers = [];

			res.on(`data`, chunk => buffers.push(chunk));

			res.on(`end`, () => {
				return aC({
					body: Buffer.concat(buffers),
					status: {
						code: res.statusCode,
						message: res.statusMessage
					},
					headers: res.headers
				});
			});
		}
	);

	req.write(request.body);
	req.end();

	return () => req.abort();
};
