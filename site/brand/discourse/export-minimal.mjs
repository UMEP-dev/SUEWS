import puppeteer from 'puppeteer';

async function run() {
  const browser = await puppeteer.launch({ headless: true });
  const page = await browser.newPage();
  await page.setViewport({ width: 800, height: 1000, deviceScaleFactor: 2 });

  await page.goto('http://127.0.0.1:9000/discourse/export-minimal.html', { waitUntil: 'networkidle0' });
  await page.evaluateHandle('document.fonts.ready');
  await new Promise(r => setTimeout(r, 500));

  const light = await page.$('#banner-light');
  await light.screenshot({ path: 'logo-banner-light.png' });
  console.log('Exported: logo-banner-light.png');

  const dark = await page.$('#banner-dark');
  await dark.screenshot({ path: 'logo-banner-dark.png' });
  console.log('Exported: logo-banner-dark.png');

  const icon512 = await page.$('#icon-512');
  await icon512.screenshot({ path: 'icon-512x512.png', omitBackground: true });
  console.log('Exported: icon-512x512.png');

  const icon120Light = await page.$('#icon-120-light');
  await icon120Light.screenshot({ path: 'icon-120x120-light.png' });
  console.log('Exported: icon-120x120-light.png');

  const icon120Dark = await page.$('#icon-120-dark');
  await icon120Dark.screenshot({ path: 'icon-120x120-dark.png' });
  console.log('Exported: icon-120x120-dark.png');

  await browser.close();
  console.log('Done!');
}

run().catch(console.error);
